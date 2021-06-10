;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'face-remap)
  (require 'flymake))

(defsubst ymacs-modeline//focus-change (&rest _)
  (if (frame-focus-state)
      (when ymacs-modeline-remap-face-cookie
        (face-remap-remove-relative ymacs-modeline-remap-face-cookie))

    (setq ymacs-modeline-remap-face-cookie
          (face-remap-add-relative 'mode-line 'mode-line-inactive)))

  (force-mode-line-update))

(defmacro ymacs-modeline//def-segment (-name &rest -body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "ymacs-modeline//segment--%s" -name)))
        (docstring (if (stringp (car -body))
                       (pop -body)
                     (format "%s modeline segment" -name))))
    (add-to-list 'ymacs-modeline-segment-alist (cons -name sym))
    `(progn
       (defsubst ,sym () ,docstring ,@-body)
       (add-to-list 'ymacs-modeline-segment-alist (cons ',-name ',sym))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(byte-compile #',sym)))))

(defun ymacs-modeline//prepare-segments (-segments)
  "Prepare mode-line `SEGMENTS'."
  (cl-loop
   for segment in -segments
   if (stringp segment)
   collect segment
   else if (symbolp segment)
   collect (if-let ((func (cdr (assq segment ymacs-modeline-segment-alist))))
               (list func)
             (error "%s is not a defined segment" segment))
   else do (error "%s is not a valid segment" segment)))

(defmacro ymacs-modeline//def-modeline (-name -segments)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `ymacs-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline -segments defined with
`ymacs-modeline//def-segment'."
  (declare (indent defun))
  (let ((sym (intern (format "ymacs-modeline//format--%s" -name))))
    `(progn
       (defsubst ,sym ()
         ,(concat "Modeline: " (prin1-to-string -segments))
         ,(cl-list* 'list (ymacs-modeline//prepare-segments -segments)))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(cl-defmacro ymacs-modeline-set!
    (-modes -key &key ((:header-line-p -header-line-p)) ((:body -body)))
  (declare (indent 2))
  (let ((fn (intern-soft (format "ymacs-modeline//format--%s" -key)))
        (format-var (if -header-line-p
                        'header-line-format
                      'mode-line-format)))
    (cl-assert (functionp fn) "predefined modeline %s doesn't exist" -key)

    (if (eq -modes 'default)
        `(setq-default ,format-var '(:eval (,fn)))

      (unless (listp -modes)
        (setq -modes (list -modes)))
      (let ((hook-fn (intern (format "ymacs-modeline//%s-setup"
                                     (mapconcat #'symbol-name -modes "/")))))
        `(progn
           (defun ,hook-fn ()
             (with-current-buffer (current-buffer)
               (setq ,format-var '(:eval (,fn))))
             ,-body)
           ,@(cl-loop
              for mode in -modes
              for name = (symbol-name mode)
              for hook = (if (string-suffix-p "mode-hook" name)
                             mode
                           (intern (concat name "-mode-hook")))
              collect
              `(add-hook ',hook ',hook-fn)))))))



;;
;;* Bar
;;

(defsubst ymacs-modeline//bar ()
  (propertize "\u200b" 'display '((height 1.3) (raise -0.15))))

;;
;;* Project
;;

(defsubst ymacs-modeline//project-root ()
  "Get the path to the root of your project.
Return nil if no project was found."
  (unless (and ymacs-modeline--project-detected-p
               (equal ymacs-modeline--project-detected-p buffer-file-name))

    (setq ymacs-modeline--project-root (ymacs-editor//project-root)
          ymacs-modeline--project-detected-p buffer-file-name)

    (when ymacs-modeline--project-root
      (setq ymacs-modeline--project-parent-path
            (abbreviate-file-name
             (file-name-directory
              (directory-file-name ymacs-modeline--project-root))))))
  ymacs-modeline--project-root)

;;
;;* Buffer information
;;

(defsubst ymacs-modeline//make-buffer-file-name ()
  "Propertized variable `buffer-file-name' given by FILE-PATH."
  (let* ((file-path (file-local-name
                     (or (buffer-file-name (buffer-base-buffer)) "")))
         (project-root (file-local-name
                        (or (ymacs-modeline//project-root)
                            default-directory)))
         (relative-path (file-relative-name
                         (or (file-name-directory file-path) "./")
                         project-root)))
    (concat
     ;; Project directory
     (propertize
      (concat (file-name-nondirectory (directory-file-name project-root)) "/")
      'face 'ymacs-modeline-project-dir)
     ;; relative path
     (propertize
      (when relative-path
        (if (string= relative-path "./")
            ""
          relative-path))
      'face 'ymacs-modeline-buffer-path)
     ;; File name
     (propertize (file-name-nondirectory file-path) 'face 'ymacs-modeline-buffer-file))))

(defsubst ymacs-modeline//update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq ymacs-modeline--buffer-file-name
        (if (buffer-file-name)
            (ymacs-modeline//make-buffer-file-name)
          '(:propertize "%b" face ymacs-modeline-buffer-file))))

(defsubst ymacs-modeline//buffer-state ()
  "current buffer state."
  (propertize " %1*%n " 'face '(:inherit ymacs-modeline-info :weight bold)))

(defun ymacs-modeline//buffer-identification (-id)
  (if (and (stringp -id)
           (= ?% (aref -id 0))
           (= ?b (aref -id (1- (length -id)))))
      (or ymacs-modeline--buffer-file-name
          (ymacs-modeline//update-buffer-file-name))
    (if (consp -id)
        (cons (ymacs-modeline//buffer-identification (car -id))
              (ymacs-modeline//buffer-identification (cdr -id)))
      -id)))

(ymacs-modeline//def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (list
   (ymacs-modeline//buffer-state)

   (when (buffer-base-buffer) "(I)")

   (ymacs-modeline//buffer-identification mode-line-buffer-identification)))

(ymacs-modeline//def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (list
   (ymacs-modeline//buffer-state)

   '(:propertize mode-line-buffer-identification face ymacs-modeline-buffer-file)))

(ymacs-modeline//def-segment buffer-info-shell
  "Display buffer information for shell buffer"
  (list
   " "
   (if (and (listp mode-line-buffer-identification)
            (equal (car mode-line-buffer-identification) "%b"))
       (cdr mode-line-buffer-identification)
     mode-line-buffer-identification)))

(ymacs-modeline//def-segment buffer-default-directory
  "Displays `default-directory' . This is for special buffers
like the scratch buffer where knowing the current project directory is important."
  (list
   (ymacs-modeline//buffer-state)

   (propertize default-directory 'face 'ymacs-modeline-buffer-path)))

;;
;;* Encoding
;;

(defsubst ymacs-modeline//update-buffer-encoding (-coding-system)
  "Update buffer file name in mode-line."
  (setq ymacs-modeline--buffer-encoding
        (concat
         ;; eol type
         (pcase (coding-system-eol-type -coding-system)
           (0 " LF")
           (1 " CRLF")
           (2 " CR")
           (_ ""))
         " "
         ;; coding system
         (let ((sys (coding-system-plist -coding-system)))
           (cond ((memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  "UTF-8")
                 (t (upcase (symbol-name (plist-get sys :name)))))))))

(ymacs-modeline//def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (or ymacs-modeline--buffer-encoding
      (ymacs-modeline//update-buffer-encoding buffer-file-coding-system)))


;;
;;* Remote host
;;

(ymacs-modeline//def-segment remote-host
  "Hostname for remote buffers."
  (or (and (not (eq ymacs-modeline--remote-host 'unset))
           ymacs-modeline--remote-host)
      ;; assume that host will not be changed after file opened
      (setq ymacs-modeline--remote-host
            (when-let ((remote (and default-directory (file-remote-p default-directory))))
              (propertize (concat "@" remote) 'face 'ymacs-modeline-host)))))

;;
;;* Major mode
;;

(defvar text-scale-mode-amount)

(ymacs-modeline//def-segment major-mode
  "The major mode, including environment and text-scale info."
  (list
   " %["
   '(:propertize mode-name face ymacs-modeline-buffer-major-mode)
   mode-line-process
   "%]"
   (and (boundp 'text-scale-mode-amount)
        (/= text-scale-mode-amount 0)
        (format
         (if (> text-scale-mode-amount 0)
             " (%+d)"
           " (%-d)")
         text-scale-mode-amount))))


;;
;;* VCS
;;

(defsubst ymacs-modeline//update-vcs (&rest _)
  "Update vcs state in mode-line."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state (vc-state buffer-file-name backend))
           (str (if vc-display-status
                    (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                  ""))
           (state-str "@")
           (face 'ymacs-modeline-info))

      (cond ((memq state '(edited added))
             (setq state-str "*"))
            ((eq state 'needs-merge)
             (setq state-str "M"))
            ((eq state 'needs-update)
             (setq state-str "U")
             (setq face 'ymacs-modeline-warning))
            ((memq state '(removed conflict unregistered))
             (setq state-str "!")
             (setq face 'ymacs-modeline-urgent)))

      (setq ymacs-modeline--vcs-state
            (propertize
             (concat " "
                     state-str
                     (if (> (length str) ymacs-modeline-vcs-max-length)
                         (concat
                          (substring str 0 (- ymacs-modeline-vcs-max-length 3))
                          "...")
                       str))
             'face face)))))

(ymacs-modeline//def-segment vcs
  "Displays the current branch, colored based on its state."
  ymacs-modeline--vcs-state)

;;
;;* Checker
;;

(defsubst ymacs-modeline//update-checker-state (&rest _)
  (setq ymacs-modeline--checker-state
        (let* ((running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (some-waiting (cl-set-difference running reported))
               (warning-level (flymake--severity :warning))
               (note-level (flymake--severity :note))
               (error-count 0)
               (warning-count 0)
               (note-count 0))
          (maphash
           (lambda (_b state)
             (dolist (diag (flymake--backend-state-diags state))
               (let ((severity (flymake--severity (flymake--diag-type diag))))
                 (cond ((> severity warning-level) (cl-incf error-count))
                       ((> severity note-level) (cl-incf warning-count))
                       (t (cl-incf note-count))))))
           flymake--backend-state)
          (cond
           (some-waiting ymacs-modeline--checker-state)
           ((zerop (hash-table-count flymake--backend-state)) "No-checker")
           ((and disabled (null running)) "All-disabled")
           ((if (> (+ error-count warning-count note-count) 0)
                (concat
                 " "
                 (propertize (number-to-string error-count) 'face 'ymacs-modeline-urgent)
                 "/"
                 (propertize (number-to-string warning-count) 'face 'ymacs-modeline-warning)
                 "/"
                 (propertize (number-to-string note-count) 'face 'ymacs-modeline-info))
              (propertize " --" 'face 'ymacs-modeline-info)))))))

(ymacs-modeline//def-segment checker
  "Displays color-coded error status in the current buffer with pretty icons."
  ymacs-modeline--checker-state)

;;
;;* Matches (macro, iedit and multi-cursors)
;;

(defvar iedit-occurrences-overlays)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function mc/num-cursors 'multiple-cursors-core)

(defsubst ymacs-modeline//macro-recording ()
  "Display current Emacs macro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    " Macro> "))

(defsubst ymacs-modeline//iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (ignore-errors
      (let ((this-oc
             (or (let ((inhibit-message t))
                   (iedit-find-current-occurrence-overlay))
                 (save-excursion
                   (iedit-prev-occurrence)
                   (iedit-find-current-occurrence-overlay))))
            (length (length iedit-occurrences-overlays))
            (sorted-ocs (sort (copy-sequence iedit-occurrences-overlays)
                              (lambda (a b) (< (overlay-start a) (overlay-start b))))))
        (format
         " %s/%d "
         (if this-oc
             (- length (1- (length (memq this-oc sorted-ocs))))
           "-")
         length)))))

(defsubst ymacs-modeline//multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (format " I %d " (mc/num-cursors))))

(ymacs-modeline//def-segment matches
  "Displays:
1. the currently recording macro
2. The number of active `iedit' regions,
3. The number of active `multiple-cursors'."
  (let ((meta (concat (ymacs-modeline//macro-recording)
                      (ymacs-modeline//iedit)
                      (ymacs-modeline//multiple-cursors))))
    (if (string-empty-p meta)
        (when size-indication-mode
          " %I")
      (concat " " (propertize meta 'face 'ymacs-modeline-panel)))))

(ymacs-modeline//def-segment buffer-size
  "Display buffer size"
  (when size-indication-mode
    " %I"))

;;
;;* Media
;;

(declare-function image-get-display-property 'image-mode)

(ymacs-modeline//def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  (when (eq major-mode 'image-mode)
    (cl-destructuring-bind (width . height)
        (when (fboundp 'image-size)
          (image-size (image-get-display-property) :pixels))
      (format " (%dx%d)" width height))))


;;
;;* Window number
;;

(defvar ace-window-mode)

(ymacs-modeline//def-segment window-number
  (when-let (number (window-parameter (selected-window) 'ace-window-path))
    (when (bound-and-true-p ymacs-x--activated)
      (setq number (propertize number 'face 'ymacs-modeline-urgent)))
    (if ace-window-mode
        (concat " " number " " (propertize ace-window-mode 'face 'aw-leading-char-face))
      (concat " " number))))


;;
;;* Misc info
;;

(defsubst ymacs-modeline//update-lsp-state (&optional -buffer)
  (let ((buffer (or -buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ymacs-modeline--lsp-state
              (format-mode-line (assoc 'lsp-mode minor-mode-alist)))))))

(defvar ymacs-desktop-name)

(ymacs-modeline//def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (list
   ""
   mode-line-misc-info
   (propertize (or ymacs-modeline--project-parent-path default-directory)
               'face 'font-lock-doc-face)
   ymacs-modeline--lsp-state
   (when ymacs-desktop-name
     (propertize ymacs-desktop-name 'face 'font-lock-constant-face))))

;;
;;* Position
;;

(ymacs-modeline//def-segment buffer-position
  "The buffer position information."
  '(line-number-mode
    ((column-number-mode " %l:%c" " %l") " " mode-line-percent-position)
    ((column-number-mode (t " %c " mode-line-percent-position)))))

;;
;;* Debug
;;

(declare-function dap-mode-line 'dap-mode)

(ymacs-modeline//def-segment debug
  "The current debug state."
  (let ((dap (or (ignore-errors (dap-mode-line))
                 (when compilation-in-progress "[Compiling]")))
        (edebug (bound-and-true-p edebug-mode)))
    (propertize
     (concat dap
             (when (and dap (or edebug debug-on-error debug-on-quit)) " ")
             (when edebug "Edebug ")
             (when debug-on-error "On-Error ")
             (when debug-on-quit "On-Quit "))
     'face 'ymacs-modeline-urgent)))

;;
;;* Git timemachine
;;

(ymacs-modeline//def-segment git-timemachine
  (concat
   " "
   ;; Snapshot icon
   (propertize "%1*" 'face '(:inherit ymacs-modeline-warning :weight normal))
   ;; Buffer name
   (propertize "*%b*" 'face 'ymacs-modeline-buffer-timemachine)))

;;
;;* Input Method
;;

(ymacs-modeline//def-segment input-method
  (when (bound-and-true-p current-input-method-title)
    (propertize (format "IM:%s " current-input-method-title)
                'face 'ymacs-modeline-input-method)))

;;
;;* Dired
;;

(ymacs-modeline//def-segment dired
  '(:propertize
    ((dired-hide-details-mode " Hide")
     (dired-omit-mode " Omit"))
    face font-lock-doc-face))




(ymacs-modeline//def-modeline main
  (window-number matches buffer-position buffer-info remote-host checker major-mode vcs buffer-encoding))

(ymacs-modeline//def-modeline shell
  (window-number matches buffer-info-shell remote-host major-mode))

(ymacs-modeline//def-modeline dired
  (window-number matches buffer-default-directory remote-host major-mode dired))

(ymacs-modeline//def-modeline vcs
  (window-number matches buffer-position buffer-info major-mode buffer-encoding))

(ymacs-modeline//def-modeline media
  (window-number buffer-size buffer-info media-info major-mode vcs))

(ymacs-modeline//def-modeline message
  (window-number matches buffer-position buffer-info-simple major-mode buffer-encoding))

(ymacs-modeline//def-modeline org-src
  (window-number matches buffer-position buffer-info-simple major-mode buffer-encoding))

(ymacs-modeline//def-modeline timemachine
  (window-number matches buffer-position git-timemachine major-mode buffer-encoding))

(ymacs-modeline//def-modeline header
  (debug input-method misc-info))

;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'hydra))

(declare-function bookmark-get-bookmark-record 'bookmark)

;;
;;* Compile
;;


(defsubst ymacs-editor//get-environment ()
  (cl-loop for fn in ymacs-editor-environment-functions
           nconc (funcall fn)))

(defun ymacs-editor//propertize-compile-command (-cmd -src-dir &optional -build-dir)
  (unless -build-dir
    (setq -build-dir -src-dir))

  (let ((cmd (concat (propertize -cmd 'cmd t)
                     (counsel-compile--pretty-propertize "in" -build-dir 'dired-directory)))
        (props `(srcdir ,-src-dir blddir ,-build-dir)))
    (add-text-properties 0 (length cmd) props cmd)
    cmd))

(defun ymacs-editor//default-compile-command (&optional -dir)
  (let ((default-directory (or -dir default-directory)))
    (mapcar
     (lambda (item)
       (apply #'ymacs-editor//propertize-compile-command item))
     (cl-loop
      for compile-command-fn in ymacs-editor-compile-command-functions
      append (funcall compile-command-fn)))))


;;
;;* Hydra
;;

(defmacro defhydra++ (name body &optional docstring &rest heads)
  "Redefine an existing hydra by adding new heads.
Arguments are same as of `defhydra'."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (setq heads (cons docstring heads))
    (setq docstring nil))
  (let ((ignore-keys (mapcar #'car (--filter (memq :delete it) heads))))
    `(defhydra ,name ,(or body (hydra--prop name "/params"))
       ,(or docstring (hydra--prop name "/docstring"))
       ,@(cl-delete-duplicates
          (cl-delete-if
           (lambda (x) (member (car x) ignore-keys))
           (append (hydra--prop name "/heads") heads))
          :key #'car
          :test #'equal))))

(defun ymacs-editor//add-toggles (-column-name -condition &rest -toggles)
  (let* ((column (assoc-string -column-name ymacs-editor-toggles-alist))
         (group (assoc -condition column))
         (toggles (cdr group)))
    (dolist (toggle (reverse -toggles))
      (setf (alist-get (car toggle) toggles nil nil #'equal)
            (cdr toggle)))
    (if group
        (setcdr group toggles)
      (setq group (cons -condition toggles))
      (if column
          (setcdr (last column) (list group))
        (setq column (list -column-name group))
        (setcdr (last ymacs-editor-toggles-alist) (list column))))))


;;
;;* Rg
;;

(defun ymacs-editor//rg-default-alias ()
  "Return the default alias by matching alias globs with the buffer file name."
  (when-let* ((buffer-name
               (or (buffer-file-name)
                   (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
              (filename
               (and buffer-name
                    (stringp buffer-name)
                    (file-name-nondirectory buffer-name))))
    (cl-find-if
     (lambda (alias)
       (string-match (mapconcat 'wildcard-to-regexp (cdr alias) "\\|")
                     filename))
     ymacs-editor-rg-type-aliases)))


;;
;;* Display Help
;;

(defsubst ymacs-editor//display-keys--collect (-keymap)
  (let (keys)
    (when -keymap
      (cl--map-keymap-recursively
       (lambda (key definition)
         (unless (or (eq (aref key 0) 'remap)  ; skip remap
                     (memq definition ymacs-editor-ivy-display-help-ignore-commands))
           (push (cons (key-description key) definition) keys)))
       -keymap))
    keys))

(defsubst ymacs-editor//display-keys--collect-extra ()
  (cl-loop for command in ymacs-editor-ivy-display-help-extra-commands
           for keys = (when (or (not (consp command))
                                (prog1 (eq (ivy-state-caller ivy-last) (car command))
                                  (setq command (cdr command))))
                          (where-is-internal command))
           when keys
           append (cl-loop
                   for key in keys
                   collect
                   (cons (key-description key) command))))

(defsubst ymacs-editor//display-keys--format (keys)
  (cl-loop
   for (definition . items) in (-group-by #'cdr keys)
   collect
   (format "[%s %s]"
           (mapconcat
            (lambda (item)
              (propertize (car item) 'face 'font-lock-builtin-face))
            items
            "/")
           (cond ((symbolp definition)
                  definition)
                 ((functionp definition)
                  "<anonymous>")
                 (t "<error>")))))

(defsubst ymacs-editor//display-help--keys ()
  (when-let ((keymap (ivy-state-keymap ivy-last))
             (keys (ymacs-editor//display-keys--format
                    (append (ymacs-editor//display-keys--collect keymap)
                            (ymacs-editor//display-keys--collect-extra)))))
    (let ((max-width (min (frame-width) ymacs-editor-ivy-display-help-max-width))
          (width 0)
          (strings))
      (dolist (key keys)
        (when (> (+ (length key) width) max-width)
          (push "\n" strings)
          (setq width 0))
        (cl-incf width (length key))
        (push key strings))
      (string-join (nreverse strings)))))

(defun ymacs-editor//display-help (&optional -cmd -directory)
  (when (and -cmd (not (stringp -cmd)))
    (setq -cmd (string-join -cmd " ")))

  (when -cmd
    (setq -cmd (propertize -cmd 'face font-lock-doc-face)))

  (let ((max-cmd-length (frame-width)))
    (when (> (length -cmd) max-cmd-length)
      (setq -cmd (concat (substring -cmd 0 max-cmd-length)
                         (propertize "[...]" 'face font-lock-keyword-face)))))

  (setq -directory
        (propertize (or -directory default-directory)
                    'face font-lock-constant-face))

  (let* ((extra-string
          (when ymacs-editor-ivy-extra-help-lines
            (propertize (string-join ymacs-editor-ivy-extra-help-lines "\n")
                        'face 'font-lock-string-face)))
         (key-string (ymacs-editor//display-help--keys))
         (cmd-string
          (when -cmd
            (format "(@%s) %s" -directory -cmd)))
         (help-string
          (concat extra-string
                  (when (and extra-string (or key-string cmd-string)) "\n")
                  key-string
                  (when (and cmd-string key-string) "\n")
                  cmd-string)))
    (unless (string-empty-p help-string)
      (lv-message "%s" help-string))))


;;
;;* Ivy
;;

(autoload 'pinyinlib-build-regexp-string "pinyinlib")

(defun ymacs-editor//ivy-re-builder (-str)
  (when (string-prefix-p "=" -str)
    (setq -str (pinyinlib-build-regexp-string (substring -str 1) t nil t)))
  (ivy--regex-plus -str))

(defmacro ymacs-editor//define-switch (&rest -body)
  (declare (indent 0))
  (let* ((commands (mapcar #'car -body))
         (props-list (mapcar #'cdr -body))
         (switch-fns (--map (intern (format "ymacs-editor//switch-to-%s" it)) commands)))
    `(progn
       ,@(cl-loop
          for (command current-switch-fn props) in (-zip commands switch-fns props-list)
          collect
          `(progn
             (defun ,current-switch-fn (&rest _)
               (interactive)
               ,@(when (plist-get props :save-text)
                   `((setq ymacs-editor-ivy--last-text ivy-text)))
               (ivy-quit-and-run
                 ,(if (plist-get props :save-text)
                      `(,command)
                    `(,command ivy-text))))
             (ivy-add-actions
              ',(or (plist-get props :caller) command)
              ',(cl-loop
                 for (switch-fn . props) in (-zip switch-fns props-list)
                 when (not (eq switch-fn current-switch-fn))
                 collect (list (plist-get props :key) switch-fn (plist-get props :doc)))))))))

(defun ymacs-editor/switch-ivy-backend ()
  (interactive)
  (let* ((caller (ivy-state-caller ivy-last))
         (toggle-fn (cl-loop
                     for (callers . toggle-fn) in ymacs-editor-ivy-switch-function-list
                     when (memq caller callers)
                     return toggle-fn)))
    (unless toggle-fn
      (user-error "No toggle-function defined"))
    (funcall toggle-fn)))


;;
;;* Transformers
;;

(defun ymacs-editor//ivy-package-install-transformer (-string)
  (let ((package (cadr (assoc-string -string package-archive-contents))))
    (concat
     (format "%-30s" -string)
     (when package
       (format " %-16s %-7s %s"
               (propertize (package-version-join (package-desc-version package))
                           'face font-lock-comment-face)
               (propertize (package-desc-archive package)
                           'face font-lock-builtin-face)
               (propertize (package-desc-summary package)
                           'face font-lock-doc-face))))))

(defun ymacs-editor//ivy-switch-buffer-transformer (-string)
  "Transform STR to more readable format."
  (let ((buffer (get-buffer -string)))
    (if (not buffer)
        -string
      (let* ((remote (buffer-local-value 'ymacs-modeline--remote-host buffer))
             (remote (and (not (eq remote 'unset)) remote))
             (face (when buffer
                     (or (when remote 'ivy-remote)
                         (when (not (verify-visited-file-modtime buffer)) 'ymacs-modeline-urgent)
                         (when (and (or (buffer-file-name buffer)
                                        (not (buffer-local-value 'buffer-read-only buffer)))
                                    (buffer-modified-p buffer))
                           'ymacs-modeline-buffer-modified)
                         (cdr (assq (buffer-local-value 'major-mode buffer)
                                    ivy-switch-buffer-faces-alist))))))
        (concat
         (format "%-60s" (ivy-append-face -string face))
         (if (buffer-file-name buffer)
             (when remote
               (propertize (format "(%s) " remote) 'face 'ymacs-modeline-host))
           (propertize
            (buffer-local-value 'default-directory buffer) 'face 'ymacs-modeline-buffer-path)))))))


(defun ymacs-editor//ivy-bookmark-transformer (-string)
  "Transform STR to more readable format."
  (let ((bm (bookmark-get-bookmark-record -string)))
    (concat
     (format "%-40s" (propertize -string 'face 'font-lock-string-face))
     (when bm
       (propertize (format "%-10d" (cdr (assoc 'position bm)))
                   'face 'warning))
     (when bm
       (file-name-nondirectory (cdr (assoc 'filename bm)))))))


;;
;;* Company
;;

(defsubst ymacs-editor//find-main-company-backend (-backends)
  (let ((x -backends))
    (while (and (consp x)
                (not (and (listp (car x)) (memq :with (car x)))))
      (setq x (cdr x)))
    x))

(cl-defun ymacs-editor//add-company-backend
    (-backend &key (-main-backend-p t) (-after nil))
  ;; deep copy the backends list
  (let ((backends (mapcar (lambda (x) (if (consp x) (copy-sequence x) x))
                          company-backends)))
    (if -main-backend-p
        (when-let (parent-of-main-backend (ymacs-editor//find-main-company-backend backends))
          ;; remove -backend first
          (setq backends (delete -backend backends))
          ;; remove 'company-capf
          (setcar parent-of-main-backend
                  (delete 'company-capf (car parent-of-main-backend)))
          (if -after
              (insert-after! -after -backend (car parent-of-main-backend))
            (cl-pushnew -backend (car parent-of-main-backend))))
      (if -after
          (insert-after! -after -backend backends)
        (cl-pushnew -backend backends)))
    (setq-local company-backends backends)))


;;
;;* Hideshow
;;

(defun ymacs-editor//hs-setup-overlay (-ov)
  (let* ((start (overlay-start -ov))
         (end (overlay-end -ov))
         (str (format " ...%d... " (count-lines start end))))
    (overlay-put -ov 'display str)
    (overlay-put -ov 'face 'ymacs-editor-hs-overlay-face)
    (overlay-put -ov 'pointer 'hand)
    (overlay-put -ov 'keymap ymacs-editor-hs-overlay-map)))

(defun ymacs-editor//hs-auto-expand (&rest _)
  (save-excursion (hs-show-block)))


;;
;;* Snippet
;;

(defun ymacs-editor//try-expand-local-snippets ()
  (-when-let*
      (((start . end)
        (when (looking-back "[a-zA-Z0-9]+" (line-beginning-position) :greedy)
          (cons (match-beginning 0) (match-end 0))))
       (template (cdr-safe (assoc-string
                            (buffer-substring-no-properties start end)
                            ymacs-editor-local-snippets-list))))
    (yas-expand-snippet template start end)))

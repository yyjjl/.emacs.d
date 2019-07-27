;;; -*- lexical-binding: t; -*-

(require 'transient)

(defun counsel//truncate-string (-string -width)
  (when (> (length -string) -width)
    (setq -string
          (concat (substring
                   (replace-regexp-in-string "\n" "\\\\n" -string)
                   0 -width)
                  " ... ")))
  -string)

(defun counsel//semantic--clean-tag (-tag)
  (let ((default-value (semantic-tag-get-attribute -tag :default-value))
        (name (semantic-tag-name -tag)))
    (when (stringp default-value)
      (semantic-tag-put-attribute -tag :default-value
                                  (counsel//truncate-string default-value 75)))
    (when (stringp name)
      (semantic-tag-set-name -tag (counsel//truncate-string name 75)))))

(defun counsel//semantic-or-imenu--relative-buffers (-buffer)
  (let* ((projectile-require-project-root nil)
         (project-buffers (ignore-errors (projectile-project-buffers))))
    (cl-loop for buffer in (buffer-list)
          when (or (eq -buffer buffer)
                   (and (buffer-file-name buffer)
                        (or (eq (buffer-local-value 'major-mode buffer)
                                (buffer-local-value 'major-mode -buffer))
                            (member buffer project-buffers))))
          collect buffer)))

(defun counsel//semantic-or-imenu--candidates (-buffers)
  (cl-loop for buffer in -buffers
        nconc
        (mapcar
         (lambda (-candidate)
           (if (null (cdr-safe -buffers))
               -candidate
             (cons (concat (buffer-name buffer) ": " (car -candidate))
                   (cdr -candidate))))
         (with-current-buffer buffer
           ;; Use semantic first
           (if (and (semantic-active-p)
                    (not (bound-and-true-p lsp-mode))) ;; use imenu when lsp-mode is enabled
               (mapcar (lambda (x)
                         (counsel//semantic--clean-tag x)
                         (cons (counsel-semantic-format-tag x) x))
                       (counsel-semantic-tags))
             (let* ((inhibit-message t)
                    (imenu-auto-rescan t)
                    (imenu-auto-rescan-maxout (if current-prefix-arg
                                                  (buffer-size)
                                                imenu-auto-rescan-maxout))
                    (items (ignore-errors (imenu--make-index-alist :noerror)))
                    (items (delete (assoc "*Rescan*" items) items)))
               (counsel-imenu-get-candidates-from items)))))))

(defun counsel//semantic-or-imenu--goto (-candidate)
  (let ((place (cdr -candidate))
        buffer goto-function)
    (if (semantic-tag-p place)
        (setq buffer (semantic-tag-buffer place)
              goto-function 'semantic-go-to-tag)
      (setq goto-function 'imenu
            buffer (let ((pos (cdr-safe place)))
                     (cond ((overlayp pos) (overlay-buffer pos))
                           ((markerp pos) (marker-buffer pos))
                           (t (current-buffer))))))
    (when buffer
      (switch-to-buffer buffer))
    (funcall goto-function place)))

;;;###autoload
(defun counsel/semantic-or-imenu* (-arg)
  "Jump to a semantic tag in the current buffer."
  (interactive "P")
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((buffer (current-buffer))
         (buffers (if -arg
                      (counsel//semantic-or-imenu--relative-buffers buffer)
                    (list buffer)))
         (candidates (counsel//semantic-or-imenu--candidates buffers)))
    (ivy-read (if -arg "All Imenu Items: " "Imenu Items: ")
              candidates
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action 'counsel//semantic-or-imenu--goto
              :caller 'counsel-semantic-or-imenu
              ;; If search for all buffers, do not jump when selecting
              ;; candidate
              :keymap (if -arg
                          counsel-imenu-map
                        (define-key! :map (make-sparse-keymap)
                          ("C-n" . ivy-next-line-and-call)
                          ("C-p" . ivy-previous-line-and-call))))))

(defvar counsel--kill-buffers nil)
(defun counsel//kill-buffer-action (x)
  (if (member x counsel--kill-buffers)
      (unless (memq this-command '(ivy-done
                                   ivy-alt-done
                                   ivy-immediate-done))
        (setq counsel--kill-buffers (delete x counsel--kill-buffers)))
    (unless (equal x "")
      (setq counsel--kill-buffers (append counsel--kill-buffers (list x)))))
  (let ((prompt (counsel//kill-buffer-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat ivy-count-format prompt))))

(defun counsel//kill-buffer-prompt (&optional -hard)
  (format "%sKill buffers (%s): "
          (if -hard "[hard] " "")
          (string-join counsel--kill-buffers ", ")))

;;;###autoload
(defun counsel/kill-buffer (-arg)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq counsel--kill-buffers nil)
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (format "%s%sKill buffers (%s): "
                      (if (eq -arg '(16)) "<hard> " "")
                      (if -arg (concat "[" (projectile-project-name) "] ") "")
                      (string-join counsel--kill-buffers ", "))
              (if -arg
                  (mapcar #'buffer-name (projectile-project-buffers))
                'internal-complete-buffer)
              :preselect (buffer-name (current-buffer))
              :action #'counsel//kill-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'counsel/kill-buffer
              :require-match t))
  (dolist (buffer counsel--kill-buffers)
    (kill-buffer buffer)))

(defun counsel//sudo-edit-file (filename &optional old-point)
  (let ((buffer (find-file
                 (if-let ((remote (file-remote-p filename)))
                     (format "%s|sudo:%s:%s"
                             (substring remote 0 (1- (length remote)))
                             (file-remote-p filename 'host)
                             (file-remote-p filename 'localname))
                   (concat "/sudo:root@localhost:" filename)))))
    (when (and (buffer-live-p buffer) old-point)
      (with-current-buffer buffer
        (goto-char old-point)))))

;;;###autoload
(defun counsel/sudo-edit (&optional -arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or -arg
          (and (not buffer-file-name)
               (not (eq major-mode 'dired-mode))))
      (ivy-read "Find file(as sudo): :" 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input default-directory
                :action
                (lambda (x)
                  (with-ivy-window
                    (counsel//sudo-edit-file (expand-file-name x ivy--directory))))
                :keymap counsel-find-file-map
                :caller 'counsel/sudo-edit)
    (counsel//sudo-edit-file (or buffer-file-name
                                 (expand-file-name default-directory))
                             (point))))

;;;###autoload
(defun swiper/dispatch (&optional -arg)
  (interactive "P")
  (call-interactively
   (cond
    ((equal -arg 0) #'isearch-forward-regexp)
    ((equal -arg 9) #'isearch-backward-regexp)
    ((equal -arg '(4)) #'swiper-isearch)
    ((equal -arg '(16)) #'swiper-multi)
    ((equal -arg '(64)) #'swiper-all)
    (t #'counsel-grep-or-swiper))))

;;;###autoload
(defun ivy-occur-filter-lines ()
  (interactive)
  (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
    (error "Current buffer is not in ivy-occur mode"))
  (let ((inhibit-read-only t)
        (regexp (read-regexp "Regexp(! for flush)"))
        (start (next-single-property-change (point-min) 'font-lock-face)))
    (funcall (if (string-prefix-p "!" regexp)
                 #'flush-lines
               #'keep-lines)
             (substring regexp 1) start (point-max) t)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (if (looking-at "stack> ")
          (forward-char 7)
        (insert "stack> "))
      (insert (format "[%s] " regexp)))))

;;;###autoload
(defun ivy-occur-undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (looking-at "stack> "))
        (undo)
      (error "Filter stack is empty"))))

(defun counsel//rg (&optional -args -extra-args)
  (interactive (list (transient-args 'counsel/rg) nil))
  (unless -args
    (setq -args '("--color never" "-M 200" "--line-number")))
  (unless (member "--pretty" -args)
    (setq -args (append -args '("--no-heading"))))

  (let ((counsel-rg-base-command (concat "rg " (string-join -args " ") " %s .")))
    (counsel-rg nil nil -extra-args)))

(defun counsel//file-jump-function (string)
  "Grep in the current directory for STRING."
  (let* ((command-args (counsel--split-command-args string))
         (search-term (cdr command-args)))
    (or
     (let ((ivy-text search-term))
       (ivy-more-chars))
     (let* ((default-directory (ivy-state-directory ivy-last))
            (regex (counsel--grep-regex search-term))
            (switches (concat (car command-args)
                              (counsel--ag-extra-switches regex)
                              (and (ivy--case-fold-p string) " -i "))))
       (counsel--async-command (counsel--format-ag-command
                                switches
                                (shell-quote-argument regex)))
       nil))))

(defclass transient-option-with-default (transient-option)
  ((value :initarg :value)))

(cl-defmethod transient-init-value ((obj transient-option-with-default))
  (oset obj value (format "%s" (oref obj value))))

(cl-defmethod transient-format-value :around ((obj transient-option-with-default))
  (let ((choices (oref obj choices))
        (value (oref obj value)))
    (if choices
        (format (propertize (concat "["
                                    (string-join
                                     (mapcar (lambda (choice)
                                               (if (equal choice value) "%s" choice))
                                             choices)
                                     "|")
                                    "]")
                            'face 'transient-inactive-value)
                (propertize value 'face 'font-lock-warning-face))
      (cl-call-next-method))))

(cl-defmethod transient-infix-read ((obj transient-option-with-default))
  (let ((choices (oref obj choices)))
    (if choices
        (or (when-let ((value (oref obj value)))
              (cadr (member value choices)))
            (car choices))
      (cl-call-next-method))))

;;;###autoload
(define-transient-command counsel/rg (&optional -directory)
  "Run ripgrep"
  ["Switches"
   ("z" "Search zip files" "--search-zip")
   ("x" "Match the whole line" "--line-regexp")
   ("v" "Show lines that do not match the given patterns" "--invert-match")
   ("l" "Only print the paths with at least one match" "--files-with-matches")
   ("L" "Follow symbolic links" "--follow")
   ("p" "Alias for '--color always --heading --line-number'" "--pretty")
   ("N" "Suppress line numbers" "--no-line-number")
   ("o" "Print only the matched (non-empty) parts" "--only-matching")
   ("I" "Don't respect ignore files" "--no-ignore")]
  ["Options"
   ("c" "Use colors" "--color "
    :class transient-option-with-default
    :choices ("auto" "ansi" "always" "never")
    :value "never")
   ("A" "After context" "--after-context " :class transient-option)
   ("B" "Before context" "--before-context " :class transient-option)
   ("C" "Show context" "--context " :class transient-option)
   ("M" "Max columns" "--max-columns " :class transient-option-with-default :value 200)]
  ["Actions"
   [("RET" "Search pattern in file" counsel//rg)]]
  (interactive
   (list
    (let ((directory (or (projectile-project-root) default-directory)))
      (or (and current-prefix-arg
               (read-directory-name "Run ripgrep in directory: " directory))
          directory))))
  (setf (nth 1 (aref (car (last (get 'counsel/rg 'transient--layout))) 2))
        (concat "Run in " (abbreviate-file-name -directory)))
  (let ((default-directory -directory))
    (if current-prefix-arg
        (transient-setup 'counsel/rg)
      (counsel//rg))))

;;;###autoload
(defun counsel/rg-file-jump ()
  (interactive)
  (let ((command "rg --glob '%s' --color never %s --files ."))
    (ivy-read "Find file (glob): "
              (lambda (string)
                (let* ((command-args (counsel--split-command-args string))
                       (search-term (cdr command-args)))
                  (or
                   (let ((ivy-text search-term)) (ivy-more-chars))
                   (progn (counsel--async-command
                           (format command search-term (car command-args)))
                          nil))))
              :matcher #'counsel--find-file-matcher
              :dynamic-collection t
              :action #'find-file
              :preselect (counsel--preselect-file)
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :require-match t
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-file-jump)))

;;;###autoload
(defun counsel/file-jump (&optional -directory -backend)
  (interactive
   (let* ((directory (or (projectile-project-root) default-directory))
          (directory (or (and current-prefix-arg
                              (read-directory-name "Jump to file in directory: " directory))
                         directory))
          (default-directory directory)
          (backends (remove nil
                            (list (and (counsel--git-root) "git")
                                  "rg"
                                  (and (projectile-project-root) "projectile"))))
          (backend (if (or (null (cdr backends))
                           (not current-prefix-arg))
                       (car backends)
                     (ivy-read "Select backends: " backends :require-match t))))
     (list directory backend)))
  (let ((default-directory -directory)
        (backend (cond
                  ((equal -backend "git") #'counsel-git)
                  ((equal -backend "projectile") #'counsel-projectile)
                  (t #'counsel/rg-file-jump))))
    (funcall backend)))

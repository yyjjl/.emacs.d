;;; -*- lexical-binding: t; -*-

(require 'transient)

(defclass transient-option-with-default (transient-option)
  ((value :initarg :value)))

(defclass transient-option-path (transient-option-with-default) ())

(cl-defmethod transient-init-value ((obj transient-option-with-default))
  (oset obj value (format "%s" (oref obj value))))

(cl-defmethod transient-format-value :around ((obj transient-option-with-default))
  (let ((choices (oref obj choices))
        (value (oref obj value)))
    (if choices
        (format
         (propertize (format "[%s]"
                             (mapconcat (lambda (choice)
                                          (if (equal choice value) "%s" choice))
                                        choices
                                        "|"))
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

(cl-defmethod transient-infix-read ((obj transient-option-path))
  (let ((default-directory (oref obj value)))
    (condition-case nil
        (read-directory-name "Path=" nil nil :must-match)
      (quit default-directory))))



(defun counsel//truncate-string (-string -width)
  (if (> (length -string) -width)
      (concat (substring
               (replace-regexp-in-string "\n" "\\\\n" -string)
               0 -width)
              " ... ")
    -string))

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
           (or (and (semantic-active-p)
                    (not (bound-and-true-p lsp-mode))
                    (ignore-errors
                      (mapcar (lambda (x)
                                (counsel//semantic--clean-tag x)
                                (cons (counsel-semantic-format-tag x) x))
                              (counsel-semantic-tags))))
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
      (unless (memq this-command '(ivy-done ivy-alt-done ivy-immediate-done))
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

(defhydra swiper-hydra (:color blue :hint nil)
  "
[_RET_/_C-s_/_m_/_a_] swiper/multi/all
[_i_/_I_]   isearch-forward(regexp)
[_r_/_R_]   isearch-backward(regexp)
"
  ("RET" swiper)
  ("C-s" swiper)
  ("i" isearch-forward)
  ("I" isearch-forward-regexp)
  ("r" isearch-backward)
  ("R" isearch-backward-regexp)
  ("m" swiper-multi)
  ("a" swiper-all))

;;;###autoload
(defun swiper/dispatch (&optional -arg)
  "if -ARG is not nil, call swiper-hydra/body else call counsel-grep-or-swiper"
  (interactive "P")
  (if -arg
      (call-interactively #'swiper-hydra/body)
    (cl-letf (((symbol-function 'swiper) #'swiper-isearch))
      (call-interactively #'counsel-grep-or-swiper))))

(defvar ivy-occur-filter-prefix ">>> ")

;;;###autoload
(defun ivy-occur/filter-lines ()
  (interactive)
  (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
    (user-error "Current buffer is not in ivy-occur mode"))

  (let ((inhibit-read-only t)
        (regexp (read-regexp "Regexp(! for flush)"))
        (start (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "[0-9]+ candidates:"))))
    (if (string-prefix-p "!" regexp)
        (flush-lines (substring regexp 1) start (point-max))
      (keep-lines regexp start (point-max)))
    (save-excursion
      (goto-char (point-min))
      (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
        (if (looking-at ivy-occur-filter-prefix)
            (progn
              (goto-char (line-end-position))
              (insert item))
          (insert ivy-occur-filter-prefix item "\n"))))))

;;;###autoload
(defun ivy-occur/undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (looking-at ivy-occur-filter-prefix))
        (save-excursion
          (undo))
      (user-error "Filter stack is empty"))))

(defun counsel//rg (&optional -args -extra-args)
  (interactive (list (transient-args 'counsel/rg) nil))
  (unless -args
    (setq -args '("--color never" "-M 1000")))
  (unless (member "--pretty" -args)
    (setq -args (append -args '("--no-heading"))))
  (unless (member "--no-line-number" -args)
    (setq -args (append -args '("--line-number"))))

  (let* ((initial-directory (cl-loop for arg in -args if (string-prefix-p "@" arg)
                                     return (substring arg 1)))
         (args (--filter (not (string-prefix-p "@" it)) -args))
         (counsel-rg-base-command (concat "rg " (string-join args " ") " %s .")))
    (let ((shell-file-name "/bin/sh"))
      (counsel-rg nil initial-directory -extra-args))))

;;;###autoload (autoload 'counsel/rg "../lisp/autoloads/ivy" nil t)
(define-transient-command counsel/rg (&optional -directory)
  "Run ripgrep"
  ["Switches"
   (3 "-h" "Search hidden files" "--hidden")
   (6 "-a" "Search binary files" "--text")
   (4 "-z" "Search zipped files" "--search-zip")
   (4 "-v" "Invert match" "--invert-match")
   (4 "-U" "Multi line" "--multiline-dotall --multiline")
   (3 "-w" "Search words" "--word-regexp")
   (5 "-x" "Search lines" "--line-regexp")
   (5 "-P" "Use PCRE2 regexps" "--pcre2")
   (4 "-1" "Don't cross file system" "--one-file-system")
   (6 "-L" "Follow symlinks" "--follow")
   (3 "-n" "Override ignore files" "--no-ignore")
   (3 "-l" "Only print the paths with at least one match" "--files-with-matches")
   (3 "-p" "Alias for '--color always --heading --line-number'" "--pretty")
   (3 "-N" "Suppress line numbers" "--no-line-number")
   (4 "-o" "Print only the matched (non-empty) parts" "--only-matching")]
  ["Options"
   ("c" "Use colors" "--color "
    :class transient-option-with-default
    :choices ("auto" "ansi" "always" "never")
    :value "never")
   (3 "A" "After context" "--after-context " :class transient-option)
   (3 "B" "Before context" "--before-context " :class transient-option)
   (3 "C" "Show context" "--context " :class transient-option)
   (3 "M" "Max columns" "--max-columns " :class transient-option-with-default :value 200)
   (4 "g" "Filter files glob" "--glob=")
   (6 "i" "Filter files glob (no case)" "--iglob=")
   (4 "T" "Exclude files types" "--type-not=")
   (4 "S" "Sort result" "--sort=")
   (5 "R" "Reverse sort result" "--sortr=")
   (6 "E" "Force encoding" "--encoding=")
   (6 "r" "Replace match" "--replace=")]
  ["Actions"
   [("SPC" "Set directory" "@=" :class transient-option-path)]
   [("RET" "Search" counsel//rg)]]
  (interactive
   (list (or (and (not current-prefix-arg) (projectile-project-root))
             default-directory)))

  (transient-insert-suffix 'counsel/rg
    "SPC"
    `("SPC" "Set directory" "@" :class transient-option-path :value ,-directory))

  (if current-prefix-arg
      (transient-setup 'counsel/rg)
    (let ((default-directory -directory))
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
          (backend (if (or (not current-prefix-arg) (= (length backends) 1))
                       (car backends)
                     (ivy-read "Select backends: " backends :require-match t))))
     (list directory backend)))
  (let ((default-directory -directory)
        (backend-fn (cond
                     ((equal -backend "git") #'counsel-git)
                     ((equal -backend "projectile") #'counsel-projectile)
                     (t #'counsel/rg-file-jump))))
    (let ((ivy-count-format (format "[%s](%%d/%%d)" -backend)))
      (funcall backend-fn))))

(defun counsel/flycheck-action-goto-error (-candidate)
  "Visit error of CANDIDATE."
  (let* ((err (cdr -candidate))
         (buffer (flycheck-error-buffer err))
         (lineno (flycheck-error-line err))
         (column (or (flycheck-error-column err) 1)))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (forward-line (1- lineno))
      (move-to-column (1- column))
      (let ((recenter-redisplay nil))
        (recenter)))))

(defun counsel//flycheck-candidate-display-string (-err)
  "Return a string of message constructed from ERROR."
  (let ((face (-> -err
                  flycheck-error-level
                  flycheck-error-level-error-list-face)))
    (format "%-8s L%-5s C%-3s %s"
            (propertize (upcase (symbol-name (flycheck-error-level -err)))
                        'font-lock-face face)
            (propertize (number-to-string (flycheck-error-line -err))
                        'face 'flycheck-error-list-line-number)
            (propertize (-if-let (column (flycheck-error-column -err))
                            (number-to-string column)
                          "0")
                        'face 'flycheck-error-list-column-number)
            (or (flycheck-error-message -err) ""))))

;;;###autoload
(defun counsel/flycheck (&optional -only-error)
  (interactive "P")
  (let ((errors (sort flycheck-current-errors #'flycheck-error-<))
        (lineno (line-number-at-pos))
        (current-point (point)))
    (when -only-error
      (setq errors
            (seq-filter (lambda (err)
                          (>=
                           (flycheck-error-level-severity (flycheck-error-level err))
                           (flycheck-error-level-severity 'error)))
                        errors)))
    (setq errors
          (mapcar (lambda (err)
                    (cons (counsel//flycheck-candidate-display-string err) err))
                  errors))
    (condition-case nil
        (ivy-read "Goto: " errors
                  :preselect
                  (car (cl-find-if
                        (lambda (err)
                          (ignore-errors (>= (flycheck-error-line (cdr err)) lineno)))
                        errors))
                  :require-match t
                  :action #'counsel/flycheck-action-goto-error
                  :keymap (define-key! :map (make-sparse-keymap)
                            ("C-n" . ivy-next-line-and-call)
                            ("C-p" . ivy-previous-line-and-call)))
      (quit (goto-char current-point)))))


;;; -*- lexical-binding: t; -*-

(defvar counsel-rg-base-command)



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
         (propertize
          (format "[%s]"
                  (mapconcat (lambda (choice) (if (equal choice value) "%s" choice))
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




;;;###autoload
(defun ymacs-counsel/semantic-or-imenu ()
  (interactive)
  (require 'semantic/fw)
  (condition-case nil
      (if (semantic-active-p)
          (progn
            (semantic-force-refresh)
            (counsel-semantic))
        (counsel-imenu))
    (error
     (ignore-errors (counsel-imenu)))))

;;* kill buffer
(defvar ymacs-counsel--buffers-to-kill nil)

(defun ymacs-counsel//kill-buffer-action (-buffer)
  (when (not (string-empty-p -buffer))
    (if (member -buffer ymacs-counsel--buffers-to-kill)
        (unless (memq this-command '(ivy-done ivy-alt-done ivy-immediate-done))
          (setq ymacs-counsel--buffers-to-kill
                (delete -buffer ymacs-counsel--buffers-to-kill)))
      (push -buffer ymacs-counsel--buffers-to-kill))))

(defun ymacs-counsel//kill-buffer-prompt ()
  (let ((prompt (ivy-state-prompt ivy-last)))
    (ivy-add-prompt-count
     (format prompt (string-join ymacs-counsel--buffers-to-kill ",")))))

;;;###autoload
(defun ymacs-counsel/kill-buffer (-project-p)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq ymacs-counsel--buffers-to-kill nil)

  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (concat (when -project-p
                        (format "[%s] " (projectile-project-name)))
                      "Kill (%s): ")
              (if -project-p
                  (mapcar #'buffer-name (projectile-project-buffers))
                'internal-complete-buffer)
              :preselect (buffer-name (current-buffer))
              :action #'ymacs-counsel//kill-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ymacs-counsel/kill-buffer
              :require-match t))

  (dolist (buffer ymacs-counsel--buffers-to-kill)
    (kill-buffer buffer)))

(ivy-set-prompt
 #'ymacs-counsel/kill-buffer
 #'ymacs-counsel//kill-buffer-prompt)

;;* swiper
(defhydra ymacs-hydra/swiper (:color blue :hint nil)
  "
[_m_] swiper-multi
[_a_] swiper-all
[_i_/_I_]   isearch-forward(regexp)
[_r_/_R_]   isearch-backward(regexp)
"
  ("i" isearch-forward)
  ("I" isearch-forward-regexp)
  ("r" isearch-backward)
  ("R" isearch-backward-regexp)
  ("m" swiper-multi)
  ("a" swiper-all))

;;;###autoload
(defun ymacs/swiper (&optional -arg)
  "if -ARG is not nil, call ymacs-hydra/swiper/body else call counsel-grep-or-swiper"
  (interactive "P")
  (call-interactively (if -arg #'ymacs-hydra/swiper/body #'counsel-grep-or-swiper)))

;;* occur filter
(defvar ymacs-ivy-occur-filter-prefix ">>> ")

;;;###autoload
(defun ymacs-ivy/occur-filter-lines ()
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
        (if (looking-at ymacs-ivy-occur-filter-prefix)
            (progn
              (goto-char (line-end-position))
              (insert item))
          (insert ymacs-ivy-occur-filter-prefix item "\n"))))))

;;;###autoload
(defun ymacs-ivy/occur-undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (looking-at ymacs-ivy-occur-filter-prefix))
        (save-excursion
          (undo))
      (user-error "Filter stack is empty"))))

;;* ripgrep

(defun ymacs-counsel//rg (&optional -args -extra-args)
  (interactive (list (transient-args 'ymacs-counsel/rg) nil))
  (unless -args
    (setq -args '("--color never" "-M 1000")))
  (unless (member "--pretty" -args)
    (setq -args (append -args '("--no-heading"))))
  (unless (member "--no-line-number" -args)
    (setq -args (append -args '("--line-number"))))

  (let* ((initial-directory (cl-loop for arg in -args if (string-prefix-p "@" arg)
                                     return (substring arg 1)))
         (args (--filter (not (string-prefix-p "@" it)) -args))
         (counsel-rg-base-command (concat "rg " (string-join args " ") " %s ."))
         (shell-file-name "/bin/sh"))
    (counsel-rg nil initial-directory -extra-args)))

;;;###autoload (autoload 'ymacs-counsel/rg "../lisp/completion-ivy/commands.el" nil t)
(define-transient-command ymacs-counsel/rg (&optional -directory)
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
   [("RET" "Search" ymacs-counsel//rg)]]
  (interactive
   (list (or (and (not current-prefix-arg) (projectile-project-root))
             default-directory)))

  (transient-insert-suffix 'ymacs-counsel/rg
    "SPC"
    `("SPC" "Set directory" "@" :class transient-option-path :value ,-directory))

  (if current-prefix-arg
      (transient-setup 'ymacs-counsel/rg)
    (let ((default-directory -directory))
      (ymacs-counsel//rg))))

;;;###autoload
(defun ymacs-ivy/select-rg-type-aliases ()
  (interactive)
  (counsel-delete-process)
  (-let* ((input ivy-text)
          (enable-recursive-minibuffers t)
          (type (ivy-read "Type: " ymacs-ivy-rg-type-aliases
                          :require-match t
                          :preselect (car (ymacs-ivy//rg-default-alias))))
          (regexp "--type=\\(\s*[^-\s]+\\)")
          ((arguments . search-parts) (split-string input " -- ")))
    (unless search-parts
      (setq search-parts (list arguments))
      (setq arguments ""))

    (setq arguments
          (string-trim
           (cond
            ((equal type "everything")
             (replace-regexp-in-string regexp "" arguments))
            ((string-match regexp arguments)
             (replace-match type nil nil arguments 1))
            (t
             (format "%s --type=%s" arguments type)))))

    (ivy-quit-and-run
      (funcall (ivy-state-caller ivy-last)
               (if (string-empty-p arguments)
                   (apply #'concat search-parts)
                 (apply #'concat arguments " -- " search-parts))))))
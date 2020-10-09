;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-counsel/semantic-or-imenu ()
  (interactive)
  (require 'semantic/fw)
  (condition-case nil
      (if (semantic-active-p)
          (counsel-semantic)
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
         (counsel-rg-base-command (concat "rg " (string-join args " ") " %s .")))
    (let ((shell-file-name "/bin/sh"))
      (counsel-rg nil initial-directory -extra-args))))

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

;;* flycheck
(defun ymacs-counsel//flycheck-candidate-display-string (-err)
  "Return a string of message constructed from ERROR."
  (let ((face (-> -err
                  flycheck-error-level
                  flycheck-error-level-error-list-face)))
    (propertize
     (format "%-8s L%-5s C%-3s %s"
             (propertize (upcase (symbol-name (flycheck-error-level -err)))
                         'font-lock-face face)
             (propertize (number-to-string (flycheck-error-line -err))
                         'face 'flycheck-error-list-line-number)
             (propertize (-if-let (column (flycheck-error-column -err))
                             (number-to-string column)
                           "0")
                         'face 'flycheck-error-list-column-number)
             (or (flycheck-error-message -err) ""))
     'error -err)))

(defun ymacs-counsel//flycheck-candidates (-only-error)
  (mapcar
   #'ymacs-counsel//flycheck-candidate-display-string
   (--filter
    (or (not -only-error)
        (>=
         (flycheck-error-level-severity (flycheck-error-level it))
         (flycheck-error-level-severity 'error)))
    flycheck-current-errors)))

;;;###autoload
(defun ymacs-counsel/flycheck (-only-error)
  "Flycheck errors."
  (interactive "P")
  (require 'flycheck)
  (let ((current-point (point))
        (lineno (line-number-at-pos))
        (errors (ymacs-counsel//flycheck-candidates -only-error)))
    (condition-case nil
        (ivy-read "flycheck errors: " errors
                  :preselect
                  (--find
                   (ignore-errors
                     (>= (flycheck-error-line (get-text-property 0 'error it))
                         lineno))
                   errors)
                  :require-match t
                  :action #'counsel-flycheck-errors-action
                  :history 'counsel-flycheck-errors-history
                  :caller 'counsel-flycheck
                  :keymap (define-key! :map (make-sparse-keymap)
                            ("C-n" . ivy-next-line-and-call)
                            ("C-p" . ivy-previous-line-and-call)))
      (quit (goto-char current-point)))))

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

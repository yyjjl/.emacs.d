;;; -*- lexical-binding: t; -*-

(require 'transient)

(defclass ymacs-transient-rg-types (transient-option)
  ())

(cl-defmethod transient-infix-read ((obj ymacs-transient-rg-types))
  (let ((default-value (oref obj value)))
    (condition-case nil
        (let ((value
               (ivy-read (format "Type(%s): " (or default-value "everything"))
                         ymacs-ivy-rg-type-aliases
                         :require-match t
                         :preselect (car (ymacs-ivy//rg-default-alias)))))
          (unless (string= value "everything")
            value))
      (quit default-value))))

(defvar counsel-rg-base-command)

;;;###autoload
(defun ymacs-ivy/meta-dot ()
  (interactive)
  (let* ((caller (ivy-state-caller ivy-last))
         (fn (intern (format "ymacs-ivy/meta-dot-for-%s" caller))))
    (if (fboundp fn)
        (funcall fn)
      (message "Not meta-dot function found for %s" caller))))


;;;###autoload
(defun ymacs-ivy/semantic-or-imenu ()
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
(defvar ymacs-ivy--buffers-to-kill nil)

(defun ymacs-ivy//kill-buffer-action (-buffer)
  (when (not (string-empty-p -buffer))
    (if (member -buffer ymacs-ivy--buffers-to-kill)
        (unless (memq this-command '(ivy-done ivy-alt-done ivy-immediate-done))
          (setq ymacs-ivy--buffers-to-kill
                (delete -buffer ymacs-ivy--buffers-to-kill)))
      (push -buffer ymacs-ivy--buffers-to-kill))))

(defun ymacs-ivy//kill-buffer-prompt ()
  (let ((prompt (ivy-state-prompt ivy-last)))
    (ivy-add-prompt-count
     (format prompt (string-join ymacs-ivy--buffers-to-kill ",")))))

;;;###autoload
(defun ymacs-ivy/kill-buffer (-project-p)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq ymacs-ivy--buffers-to-kill nil)

  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (concat (when -project-p
                        (format "[%s] " (projectile-project-name)))
                      "Kill (%s): ")
              (if -project-p
                  (mapcar #'buffer-name (projectile-project-buffers))
                'internal-complete-buffer)
              :preselect (buffer-name (current-buffer))
              :action #'ymacs-ivy//kill-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ymacs-ivy/kill-buffer
              :require-match t))

  (dolist (buffer ymacs-ivy--buffers-to-kill)
    (kill-buffer buffer)))

(ivy-set-prompt
 #'ymacs-ivy/kill-buffer
 #'ymacs-ivy//kill-buffer-prompt)

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
(defvar ymacs-ivy--last-text nil)

(defun ymacs-ivy//rg-in-directory (&optional -args -directory -extra-args)
  (interactive (list (transient-args 'ymacs-ivy/rg) nil nil))

  (cl-pushnew "--no-heading" -args)
  (cl-pushnew "--color=never" -args)

  (when (and (not (or (member "-z" -args)
                      (member "--search-zip" -args)))
             (stringp (buffer-file-name))
             (or (string-suffix-p ".gz" (buffer-file-name))
                 (string-suffix-p ".zip" (buffer-file-name))))
    (push "--search-zip" -args))

  (unless (--some (or (string-prefix-p "-M" it)
                      (string-prefix-p "--max-columns" it))
                  -args)
    (push "--max-columns=1000" -args))

  (unless (member "--no-line-number" -args)
    (cl-pushnew "--line-number" -args))

  (let* ((initial-directory (or -directory (read-directory-name "Direcotry: ")))
         (counsel-rg-base-command (concat "rg " (string-join -args " ") " %s ."))
         (shell-file-name "/bin/sh"))
    (ymacs-ivy//display-help "<empty>" initial-directory)
    (counsel-rg ymacs-ivy--last-text initial-directory -extra-args)))

(defun ymacs-ivy//rg (&optional -args)
  (interactive (list (transient-args 'ymacs-ivy/rg)))

  (ymacs-ivy//rg-in-directory -args default-directory))

;;;###autoload (autoload 'ymacs-ivy/rg "../lisp/completion-ivy/commands.el" nil t)
(transient-define-prefix ymacs-ivy/rg ()
  "Run ripgrep"
  [["Switches"
    (3 "--" "Default extra options" "--search-zip --no-ignore")
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
    ;; (3 "-p" "Alias for '--color always --heading --line-number'" "--pretty")
    (3 "-N" "Suppress line numbers" "--no-line-number")
    (4 "-o" "Print only the matched (non-empty) parts" "--only-matching")]
   ["Options"
    (3 "A" "After context" "--after-context=")
    (3 "B" "Before context" "--before-context=")
    (3 "C" "Show context" "--context=")
    (3 "M" "Max columns" "--max-columns=")
    (4 "g" "Filter files glob" "--glob=")
    (6 "i" "Filter files glob (no case)" "--iglob=")
    (3 "t" "Include files types" "--type=" :class ymacs-transient-rg-types)
    (4 "T" "Exclude files types" "--type-not=")
    (4 "S" "Sort result" "--sort=")
    (5 "R" "Reverse sort result" "--sortr=")
    (6 "E" "Force encoding" "--encoding=")
    (6 "r" "Replace match" "--replace=")]]
  [:description
   (lambda () (format "Actions in %s" (abbreviate-file-name default-directory)))
   [("SPC" "Search in another directory" ymacs-ivy//rg-in-directory)]
   [("RET" "Search" ymacs-ivy//rg)]]
  (interactive)

  (setq ymacs-ivy--last-text nil)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (ymacs-ivy//rg)))

;;;###autoload
(defun ymacs-ivy/meta-dot-for-counsel-rg ()
  (interactive)
  (counsel-delete-process)
  (setq ymacs-ivy--last-text ivy-text)
  (ivy-quit-and-run
    (transient-setup 'ymacs-ivy/rg)))

;;* fzf

(defun ymacs-ivy//fzf (&optional -args)
  (interactive (list (transient-args 'ymacs-ivy/fzf)))

  (unless (--some (or (string-prefix-p "-t" it)
                      (string-prefix-p "--type" it))
                  -args)
    (push "--type=file" -args))

  (let ((ymacs-ivy-additional-environment
         (list (format "FZF_DEFAULT_COMMAND=%s %s" ymacs-fdfind-path (string-join -args " ")))))
    (with-temp-env! ymacs-ivy-additional-environment
      (counsel-fzf ymacs-ivy--last-text))))

(transient-define-argument ymacs-ivy/fzf-file-type ()
  :description "Filter search by type"
  :class 'transient-switches
  :key "t"
  :argument-format "%s"
  :argument-regexp "--type=\\([a-zA-Z]+?\\)"
  :choices '("file" "directory" "symlink" "executable" "empty"))

;;;###autoload (autoload 'ymacs-ivy/fzf "../lisp/completion-ivy/commands.el" nil t)
(transient-define-prefix ymacs-ivy/fzf ()
  "Run fzf"
  ["Switches"
   (3 "--" "Default extra options" "--hidden --no-ignore --follow")
   (3 "-h" "Search hidden files" "--hidden")
   (3 "-I" "Show all search results" "--no-ignore")
   (4 "-L" "Follow symbol links" "--follow")]
  ["Options"
   (ymacs-ivy/fzf-file-type)
   (3 "d" "Limit directory traversal to at most d levels of depth" "--max-depth=")
   (4 "S" "Limit results based on the size(b/k/m/g)" "--size=")
   (6 "E" "Exclude files/directories that match the given glob pattern" "--exclude=")]
  ["Actions"
   [("RET" "Search" ymacs-ivy//fzf)]]
  (interactive)

  (setq ymacs-ivy--last-text nil)
  (ymacs-ivy//fzf))

;;;###autoload
(defun ymacs-ivy/meta-dot-for-counsel-fzf ()
  (interactive)
  (counsel-delete-process)
  (setq ymacs-ivy--last-text ivy-text)
  (ivy-quit-and-run
    (transient-setup 'ymacs-ivy/fzf)))

;;; -*- lexical-binding: t; -*-

(require 'transient)

(defclass ymacs-transient-rg-types (transient-option)
  ())

(cl-defmethod transient-infix-read ((obj ymacs-transient-rg-types))
  (let ((default-value (oref obj value)))
    (condition-case nil
        (let ((value
               (ivy-read (format "Type(%s): " (or default-value "everything"))
                         ymacs-editor-rg-type-aliases
                         :require-match t
                         :preselect (car (ymacs-editor//rg-default-alias)))))
          (unless (string= value "everything")
            value))
      (quit default-value))))

(defvar counsel-rg-base-command)

;;;###autoload
(defun ymacs-editor/ivy-meta-dot ()
  (interactive)
  (let* ((caller (ivy-state-caller ivy-last))
         (fn (intern (format "ymacs-editor/ivy-meta-dot-for-%s" caller))))
    (if (fboundp fn)
        (funcall fn)
      (message "Not meta-dot function found for %s" caller))))

;;* swiper
(defhydra ymacs-hydra/swiper (:color blue :hint nil)
  "
[_m_] swiper-multi
[_a_] swiper-all
[_i_/_I_] isearch-forward(regexp)
[_r_/_R_] isearch-backward(regexp)
"
  ("i" isearch-forward)
  ("I" isearch-forward-regexp)
  ("r" isearch-backward)
  ("R" isearch-backward-regexp)
  ("m" swiper-multi)
  ("a" swiper-all))

;;;###autoload
(defun ymacs-editor/swiper (&optional -arg)
  "if -ARG is not nil, call ymacs-hydra/swiper/body else call counsel-grep-or-swiper"
  (interactive "P")
  (call-interactively (if -arg #'ymacs-hydra/swiper/body #'counsel-grep-or-swiper)))

;;* ripgrep

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

(defun ymacs-editor//rg-in-directory (&optional -args -directory -extra-args)
  (interactive (list (transient-args 'ymacs-editor/rg) nil nil))

  (when (and (not (member "-z" -args))
             (buffer-file-name)
             (or (string-suffix-p ".gz" (buffer-file-name))
                 (string-suffix-p ".zip" (buffer-file-name))
                 (>= (prefix-numeric-value current-prefix-arg) 4)))
    (cl-pushnew "--search-zip" -args))

  (let* ((initial-directory
          (or -directory
              (let ((default-directory (or ymacs-editor-ivy--last-directory default-directory)))
                (setq ymacs-editor-ivy--last-directory
                      (read-directory-name "Direcotry: ")))))
         (shell-file-name "/bin/sh"))
    (counsel-rg ymacs-editor-ivy--last-text
                initial-directory
                (string-join (append -args -extra-args) " "))))

;;;###autoload
(defun ymacs-editor//rg (&optional -args)
  (interactive (list (transient-args 'ymacs-editor/rg)))

  (ymacs-editor//rg-in-directory
   -args
   (or ymacs-editor-ivy--last-directory
       ymacs-editor-search-directory
       (ymacs-editor//project-root-or-default))))

(transient-define-prefix ymacs-editor/rg ()
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
    (4 "-o" "Print only the matched (non-empty) parts" "--only-matching")]
   ["Options"
    (3 "A" "After context" "--after-context=")
    (3 "B" "Before context" "--before-context=")
    (3 "C" "Show context" "--context=")
    (4 "g" "Filter files glob" "--glob=")
    (6 "i" "Filter files glob (no case)" "--iglob=")
    (3 "t" "Include files types" "--type=" :class ymacs-transient-rg-types)
    (4 "T" "Exclude files types" "--type-not=")
    (4 "S" "Sort result" "--sort=")
    (5 "R" "Reverse sort result" "--sortr=")
    (6 "E" "Force encoding" "--encoding=")
    (6 "r" "Replace match" "--replace=")]]
  [:description
   (lambda ()
     (format "Actions [%s]" (or ymacs-editor-ivy--last-directory default-directory)))
   [("SPC" "Choose a direcotry to search" ymacs-editor//rg-in-directory)]
   [("RET" "Search in project root" ymacs-editor//rg)]]
  (interactive)

  (setq ymacs-editor-ivy--last-text nil)
  (setq ymacs-editor-ivy--last-directory nil)
  (if current-prefix-arg
      (transient-setup 'ymacs-editor/rg)
    (ymacs-editor//rg)))

;;;###autoload
(defun ymacs-editor/ivy-meta-dot-for-counsel-rg ()
  (interactive)
  (counsel-delete-process)
  (setq ymacs-editor-ivy--last-text ivy-text)
  (ivy-quit-and-run
    (transient-setup 'ymacs-editor/rg)))

;;* fzf

;;;###autoload
(defun ymacs-editor//fzf (&optional -args)
  (interactive (list (transient-args 'ymacs-editor/fzf)))

  (unless (--some (or (string-prefix-p "-t" it)
                      (string-prefix-p "--type" it))
                  -args)
    (push "--type=file" -args))

  (let ((ymacs-editor-display-help-extra-lines
         (list (format "FZF_DEFAULT_COMMAND=%s %s"
                       (or (when-let (cmd (getenv "FZF_DEFAULT_COMMAND"))
                             (car (split-string cmd)))
                           "fd")
                       (string-join -args " ")))))
    (with-temp-env! ymacs-editor-display-help-extra-lines
      (counsel-fzf ymacs-editor-ivy--last-text))))

(transient-define-argument ymacs-editor/fzf-file-type ()
  :description "Filter search by type"
  :class 'transient-switches
  :key "t"
  :argument-format "%s"
  :argument-regexp "--type=\\([a-zA-Z]+?\\)"
  :choices '("file" "directory" "symlink" "executable" "empty"))

(transient-define-prefix ymacs-editor/fzf ()
  "Run fzf"
  ["Switches"
   (3 "--" "Default extra options" "--hidden --no-ignore --follow")
   (3 "-h" "Search hidden files" "--hidden")
   (3 "-I" "Show all search results" "--no-ignore")
   (4 "-L" "Follow symbol links" "--follow")]
  ["Options"
   (ymacs-editor/fzf-file-type)
   (3 "d" "Limit directory traversal to at most d levels of depth" "--max-depth=")
   (4 "S" "Limit results based on the size(b/k/m/g)" "--size=")
   (6 "E" "Exclude files/directories that match the given glob pattern" "--exclude=")]
  ["Actions"
   [("RET" "Search" ymacs-editor//fzf)]]
  (interactive)

  (setq ymacs-editor-ivy--last-text nil)
  (ymacs-editor//fzf))

;;;###autoload
(defun ymacs-editor/ivy-meta-dot-for-counsel-fzf ()
  (interactive)
  (counsel-delete-process)
  (setq ymacs-editor-ivy--last-text ivy-text)
  (ivy-quit-and-run
    (transient-setup 'ymacs-editor/fzf)))

;;;###autoload
(defun ymacs-editor/compile (-noninteractive)
  (interactive "P")
  (let* ((root (or (counsel--compile-root) default-directory))
         (ymacs-editor-display-help-extra-lines
          (list (format "%s @ %s"
                        (if -noninteractive "Comint" "Compilation")
                        root))))
    (with-transient-advice!
        (compile :around (-fn -cmd &optional _)
                 (funcall -fn -cmd (not -noninteractive)))
      (counsel-compile root))))

;;;###autoload
(defun ymacs-editor/smart-counsel-cd ()
  (interactive)
  (counsel-delete-process)
  (let* ((input ivy-text)
         (enable-recursive-minibuffers t)
         (directory (buffer-file-name (ivy-state-buffer ivy-last)))
         (directory (and directory (file-name-directory directory)))
         (candidates
          (when directory
            (let ((default-directory directory))
              (cl-remove-duplicates
               (thread-last counsel-compile-root-functions
                 (mapcar #'funcall)
                 (remove nil)
                 (remove (expand-file-name (ivy-state-directory ivy-last)))
                 (mapcar #'expand-file-name))
               :test #'string=))))
         (new-directory
          (if candidates
              (completing-read!
               "cd: "
               candidates
               ;; return
               (lambda () (counsel-read-directory-name "cd: " directory))
               ;; return-prompt
               "read-interactively")
            (counsel-read-directory-name "cd: " directory))))
    (ivy-quit-and-run
      (funcall (ivy-state-caller ivy-last) input new-directory))))

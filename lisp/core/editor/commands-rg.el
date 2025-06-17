;;; -*- lexical-binding: t; -*-

(require 'transient)

(defvar ymacs-editor--ripgrep-settings-file (expand-cache! "transient/rigrep-settings.el"))

(defvar ymacs-editor--ripgrep-current-buffer nil)
(defvar ymacs-editor--ripgrep-settings (transient--read-file-contents ymacs-editor--ripgrep-settings-file))

(define-hook! ymacs-editor//ripgrep-save-to-file (kill-emacs-hook)
  (transient--pp-to-file ymacs-editor--ripgrep-settings ymacs-editor--ripgrep-settings-file))

(defmacro ymacs-editor//ripgrep-get-var (-var)
  `(let ((buffer (or ymacs-editor--ripgrep-current-buffer (current-buffer))))
     (plist-get (cdr (assoc (buffer-file-name buffer) ymacs-editor--ripgrep-settings)) ,-var)))

(defmacro ymacs-editor//ripgrep-set-var (-var -value)
  `(let* ((buffer (or ymacs-editor--ripgrep-current-buffer (current-buffer)))
          (file-name (buffer-file-name buffer))
          (item (assoc file-name ymacs-editor--ripgrep-settings))
          (new-list (plist-put (cdr item) ,-var ,-value)))
     (unless (null file-name)
       (if item
           (setf (cdr item) new-list)
         (push (cons file-name new-list) ymacs-editor--ripgrep-settings)))))

(defsubst ymacs-editor//ripgrep-working-directory ()
  (let ((remote-host (file-remote-p default-directory))
        (working-directory (or (ymacs-editor//ripgrep-get-var :working-dir)
                               ymacs-editor-search-directory
                               (ymacs-editor//project-root-or-default))))
    (if (file-remote-p working-directory)
        working-directory
      (if remote-host
          (concat remote-host working-directory)
        working-directory))))

(defun ymacs-editor//ripgrep-default-alias ()
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

(defun ymacs-editor//read-ripgrep-types (-prompt -initial-input -history)
  (completing-read -prompt ymacs-editor-rg-type-aliases nil :require-match -initial-input -history))

(defmacro ymacs-editor//define-ripgrep-switch (-name -doc -shortarg -argument)
  (declare (indent 1))
  `(transient-define-infix ,(intern (format "ymacs-editor--ripgrep-%s" -name)) ()
     :description ,-doc
     :class 'transient-switch
     :shortarg ,-shortarg
     :argument ,-argument
     :init-value (lambda (-obj)
                   (let ((cmdargs (ymacs-editor//ripgrep-get-var :cmdargs)))
                     (oset -obj value (when (or (member ,-shortarg cmdargs)
                                                (member ,-argument cmdargs))
                                        ,-argument))))))

;;;###autoload
(defun ymacs-editor/meta-dot-for-ripgrep ()
  (interactive)
  (unless (active-minibuffer-window)
    (user-error "minibuffer is not active"))

  (-let* ((input (with-selected-window (active-minibuffer-window)
                   (minibuffer-contents-no-properties)))
          (split-fn (plist-get (consult--async-split-style) :function))
          ((search-term emacs-input-start-pos . _) (funcall split-fn input))
          (emacs-filter (substring input emacs-input-start-pos))
          ((search-term . cmdargs) (consult--command-split search-term)))
    ;; (ymacs-editor//ripgrep-set-var :working-dir default-directory)
    (when cmdargs
      (ymacs-editor//ripgrep-set-var :cmdargs cmdargs))
    (ymacs-editor//ripgrep-set-var :search-term search-term)
    (ymacs-editor//ripgrep-set-var :emacs-filter emacs-filter))

  (ymacs-editor//minibuffer-quit-and-run
    (transient-setup 'ymacs-editor/ripgrep)))


(defun ymacs-editor//ripgrep-1 (&optional -cmdargs &rest _ignored)
  (interactive (list (transient-args 'ymacs-editor/ripgrep) nil nil))

  ;; remove directory
  (let ((search-term (ymacs-editor//ripgrep-get-var :search-term))
        (emacs-filter (ymacs-editor//ripgrep-get-var :emacs-filter))
        (working-dir (ymacs-editor//ripgrep-get-var :working-dir))
        cmdargs)
    (dolist (arg -cmdargs)
      (if (string-prefix-p "@" arg)
          (let ((tokens (split-string arg "=")))
            (pcase (car tokens)
              ("@term" (setq search-term (cadr tokens)))
              ("@filter" (setq emacs-filter (cadr tokens)))
              ("@directory" (setq working-dir (cadr tokens)))))
        (push arg cmdargs)))

    (when (and (not (member "-z" cmdargs))
               (buffer-file-name)
               (or (string-suffix-p ".gz" (buffer-file-name))
                   (string-suffix-p ".zip" (buffer-file-name))))
      (cl-pushnew "--search-zip" cmdargs))

    (ymacs-editor//ripgrep-set-var :cmdargs cmdargs)
    (ymacs-editor//ripgrep-set-var :search-term search-term)
    (ymacs-editor//ripgrep-set-var :emacs-filter emacs-filter)
    (ymacs-editor//ripgrep-set-var :working-dir working-dir)

    (let* ((switches (string-join cmdargs " "))
           (enable-recursive-minibuffers t)
           (shell-file-name "/bin/sh")
           (this-command #'consult-ripgrep)
           (input (cond ((and emacs-filter
                              (not (string-empty-p emacs-filter))
                              (not (string-empty-p switches)))
                         (format "%s -- %s#%s" (if (string-empty-p search-term) "" search-term) switches emacs-filter))
                        ((and (not (string-empty-p switches)))
                         (format "%s -- %s" (if (string-empty-p search-term) "" search-term) switches))
                        ((and (not (string-empty-p emacs-filter)))
                         (format "%s#%s" search-term emacs-filter))
                        (t
                         search-term))))
      (consult-ripgrep working-dir input))))

(defmacro ymacs-editor//define-ripgrep-option (-name -doc -shortarg -argument &optional -reader)
  (declare (indent 1))
  `(transient-define-infix ,(intern (format "ymacs-editor--ripgrep-%s" -name)) ()
     :description ,-doc
     :class 'transient-option
     :shortarg ,-shortarg
     :argument ,-argument
     :always-read t
     :reader ',-reader
     :init-value
     (lambda (-obj)
       (let ((options (ymacs-editor//ripgrep-get-var :cmdargs)))
         (oset -obj value
               (cl-loop for option in options
                        when (string-prefix-p ,-argument option)
                        return (nth 1 (split-string option "="))))))))

(ymacs-editor//define-ripgrep-switch search-hidden "Search hidden files" "-h" "--hidden")
(ymacs-editor//define-ripgrep-switch search-zip "Search zipped files" "-z" "--search-zip")
(ymacs-editor//define-ripgrep-switch follow-symlink "Follow symlinks" "-L" "--follow")
(ymacs-editor//define-ripgrep-switch no-ignore "Override ignore files" "-n" "--no-ignore")

(ymacs-editor//define-ripgrep-option glob "Filter files glob" "g" "--glob=")
(ymacs-editor//define-ripgrep-option iglob "Filter files glob (no case)" "I" "--iglob=")
(ymacs-editor//define-ripgrep-option type "Filter files glob (no case)" "t" "--type=" ymacs-editor//read-ripgrep-types)

(transient-define-infix ymacs-editor--ripgrep-directory ()
  :description "Set working directory"
  :class 'transient-option
  :shortarg "d"
  :argument "@directory="
  :prompt "Directory: "
  :always-read t
  :reader
  (lambda (-prompt -initial-input _history)
    (read-directory-name -prompt nil nil t -initial-input))
  :init-value
  (lambda (-obj)
    (oset -obj value (ymacs-editor//ripgrep-working-directory))))

(transient-define-prefix ymacs-editor/ripgrep ()
  "Run ripgrep"
  [["Switches"
    (ymacs-editor--ripgrep-search-hidden)
    (ymacs-editor--ripgrep-search-zip)
    (ymacs-editor--ripgrep-follow-symlink)
    (ymacs-editor--ripgrep-no-ignore)]
   ["Options"
    (ymacs-editor--ripgrep-glob)
    (ymacs-editor--ripgrep-iglob)
    (ymacs-editor--ripgrep-type)
    (ymacs-editor--ripgrep-directory)]]
  ["Input&Filter"
   ("i" "Search term" "@term="
    :prompt "term: "
    :always-read t
    :init-value (lambda (-obj) (oset -obj value (ymacs-editor//ripgrep-get-var :search-term))))
   ("f" "Emacs filter" "@filter="
    :prompt "filter: "
    :always-read t
    :init-value (lambda (-obj) (oset -obj value (ymacs-editor//ripgrep-get-var :emacs-filter))))]
  ["Action"
   [("RET" "Do search" ymacs-editor//ripgrep-1)]]

  (interactive)
  (setq ymacs-editor--ripgrep-current-buffer (current-buffer))

  (if current-prefix-arg
      (transient-setup 'ymacs-editor/ripgrep)
    (let ((shell-file-name "/bin/sh")
          (this-command #'consult-ripgrep))
      (consult-ripgrep (ymacs-editor//ripgrep-working-directory)))))

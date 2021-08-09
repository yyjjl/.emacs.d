;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-ripgrep-current-buffer nil)
(defvar-local ymacs-editor-ripgrep-input nil)

(defsubst ymacs-editor//ripgrep-field (-index -default)
  (with-current-buffer (or ymacs-editor-ripgrep-current-buffer (current-buffer))
    (or (nth -index ymacs-editor-ripgrep-input) -default)))

(defsubst ymacs-editor//set-ripgrep-field (-index -value)
  (with-current-buffer (or ymacs-editor-ripgrep-current-buffer (current-buffer))
    (unless ymacs-editor-ripgrep-input
      (setq ymacs-editor-ripgrep-input (list nil nil nil nil)))
    (setf (nth -index ymacs-editor-ripgrep-input) -value)))

(defsubst ymacs-editor//ripgrep-input-1 ()
  (ymacs-editor//ripgrep-field 0 ""))
(defsubst ymacs-editor//ripgrep-input-2 ()
  (ymacs-editor//ripgrep-field 1 ""))
(defsubst ymacs-editor//ripgrep-switches ()
  (ymacs-editor//ripgrep-field 2 nil))
(defsubst ymacs-editor//ripgrep-directory ()
  (ymacs-editor//ripgrep-field 3 nil))

(defsubst ymacs-editor//set-ripgrep-input-1 (-input-1)
  (ymacs-editor//set-ripgrep-field 0 -input-1))
(defsubst ymacs-editor//set-ripgrep-input-2 (-input-2)
  (ymacs-editor//set-ripgrep-field 1 -input-2))
(defsubst ymacs-editor//set-ripgrep-switches (-switches)
  (ymacs-editor//set-ripgrep-field 2 -switches))
(defsubst ymacs-editor//set-ripgrep-directory (-directory)
  (ymacs-editor//set-ripgrep-field 3 -directory))

(defsubst ymacs-editor//ripgrep-working-directory ()
  (or (ymacs-editor//ripgrep-directory)
      ymacs-editor-search-directory
      (ymacs-editor//project-root-or-default)))




(require 'transient)

(defclass ymacs-transient-ripgrep-types (transient-option)
  ())

(defclass ymacs-transient-ripgrep-directory (transient-option)
  ())

(cl-defmethod transient-infix-read ((obj ymacs-transient-ripgrep-types))
  (let ((default-value (oref obj value)))
    (condition-case nil
        (let ((values (completing-read-multiple
                       (format "Type (%s): " (or default-value "everything"))
                       ymacs-editor-rg-type-aliases
                       nil
                       :require-match
                       default-value)))
          (unless (member "everything" values)
            (string-join values ",")))
      (quit default-value))))

(cl-defmethod transient-infix-read ((obj ymacs-transient-ripgrep-directory))
  (let ((default-value (oref obj value)))
    (condition-case nil
        (let ((directory (read-directory-name "Directory: " default-value nil t)))
          (ymacs-editor//set-ripgrep-directory directory)
          directory)
      (quit default-value))))




(defmacro ymacs-editor//minibuffer-quit-and-run (&rest -body)
  "Quit the minibuffer and run BODY afterwards."
  (declare (indent 0))
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@-body)))
     (abort-recursive-edit)))

;;;###autoload
(defun ymacs-editor//next-history-element (-arg)
  (interactive "p")
  (if (and (= minibuffer-history-position 0)
           (= (minibuffer-prompt-end) (point)))
      (let ((buffer (window-buffer (minibuffer-selected-window))))
        (insert (or (with-current-buffer buffer
                      (save-excursion
                        (when ymacs-editor-minibuffer-saved-point
                          (goto-char ymacs-editor-minibuffer-saved-point))
                        (thing-at-point 'symbol)))
                    "")))
    (next-history-element -arg)))


(defsubst ymacs-editor//minibuffer-completing-file-p ()
  "Return non-nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end) (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

(defun ymacs-editor//minibuffer-up-directory ()
  (when (and (ymacs-editor//minibuffer-completing-file-p)
             (eolp)
             (eq (char-before) ?/))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))
    t))

;;;###autoload
(defun ymacs-editor//minibuffer-delete-char ()
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (user-error "No in minibuffer"))

  (unless (ymacs-editor//minibuffer-up-directory)
    (call-interactively #'delete-backward-char)))

;;;###autoload
(defun ymacs-editor//minibuffer-delete-word ()
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (user-error "No in minibuffer"))

  (unless (ymacs-editor//minibuffer-up-directory)
    (call-interactively #'backward-kill-word)))


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

;;;###autoload
(defun ymacs-editor/meta-dot-for-ripgrep ()
  (interactive)
  (unless (active-minibuffer-window)
    (user-error "minibuffer is not active"))

  (-let* ((input (with-selected-window (active-minibuffer-window)
                   (minibuffer-contents-no-properties)))
          ((rg-input emacs-input . _) (consult--split-perl input 0))
          ((rg-real-input . rg-switches) (consult--command-split rg-input)))
    (ymacs-editor//set-ripgrep-input-1 rg-real-input)
    (ymacs-editor//set-ripgrep-input-2 emacs-input)
    (ymacs-editor//set-ripgrep-switches rg-switches)
    (ymacs-editor//set-ripgrep-directory default-directory))

  (ymacs-editor//minibuffer-quit-and-run
    (transient-setup 'ymacs-editor/ripgrep)))

(defun ymacs-editor//ripgrep-internal (&optional -switches &rest _ignored)
  (interactive (list (transient-args 'ymacs-editor/ripgrep) nil nil))

  ;; remove directory
  (setq -switches (or -switches (ymacs-editor//ripgrep-switches)))
  (setq -switches (cl-remove-if (lambda (x) (string-prefix-p "@" x)) -switches))

  (when (and (not (member "-z" -switches))
             (buffer-file-name)
             (or (string-suffix-p ".gz" (buffer-file-name))
                 (string-suffix-p ".zip" (buffer-file-name))
                 (>= (prefix-numeric-value current-prefix-arg) 4)))
    (cl-pushnew "--search-zip" -switches))

  (ymacs-editor//set-ripgrep-switches -switches)

  (let ((rg-input (ymacs-editor//ripgrep-input-1))
        (emacs-input (ymacs-editor//ripgrep-input-2))
        (switches (string-join -switches " "))

        (enable-recursive-minibuffers t)
        (shell-file-name "/bin/sh")
        (this-command #'consult-ripgrep))
    (consult-ripgrep
     (ymacs-editor//ripgrep-working-directory)
     (cond ((and (not (string-empty-p emacs-input))
                 (not (string-empty-p switches)))
            (format "%s -- %s#%s" (if (string-empty-p rg-input) "_" rg-input) switches emacs-input))
           ((and (not (string-empty-p switches)))
            (format "%s -- %s" (if (string-empty-p rg-input) "_" rg-input) switches))
           ((and (not (string-empty-p emacs-input)))
            (format "%s#%s" rg-input emacs-input))
           (t
            rg-input)))))

(defmacro ymacs-editor//define-ripgrep-infix-1 (-name -doc -shortarg -argument)
  (declare (indent 1))
  `(transient-define-infix ,(intern (format "ymacs-editor-ripgrep-%s" -name)) ()
     :description ,-doc
     :class 'transient-switch
     :shortarg ,-shortarg
     :argument ,-argument
     :init-value (lambda (-obj)
                   (let ((switchs (ymacs-editor//ripgrep-switches)))
                     (oset -obj value (when (or (member ,-shortarg switchs)
                                                (member ,-argument switchs))
                                        ,-argument))))))

(defmacro ymacs-editor//define-ripgrep-infix-2 (-name -doc -shortarg -argument &optional -class)
  (declare (indent 1))
  `(transient-define-infix ,(intern (format "ymacs-editor-ripgrep-%s" -name)) ()
     :description ,-doc
     :class ,(or -class 'transient-option)
     :shortarg ,-shortarg
     :argument ,-argument
     :init-value
     (lambda (-obj)
       (let ((options (ymacs-editor//ripgrep-switches)))
         (oset -obj value
               (cl-loop for option in options
                        when (string-prefix-p ,-argument option)
                        return (nth 1 (split-string option "="))))))))

(ymacs-editor//define-ripgrep-infix-1 search-hidden "Search hidden files" "--" "--hidden")
(ymacs-editor//define-ripgrep-infix-1 search-zip "Search zipped files" "-z" "--search-zip")
(ymacs-editor//define-ripgrep-infix-1 follow-symlink "Follow symlinks" "-L" "--follow")
(ymacs-editor//define-ripgrep-infix-1 no-ignore "Override ignore files" "-n" "--no-ignore")

(ymacs-editor//define-ripgrep-infix-2 glob "Filter files glob" "g" "--glob=")
(ymacs-editor//define-ripgrep-infix-2 iglob "Filter files glob (no case)" "i" "--iglob=")
(ymacs-editor//define-ripgrep-infix-2 type "Filter files glob (no case)" "t" "--type=" ymacs-transient-ripgrep-types)

(transient-define-infix ymacs-editor-ripgrep-directory ()
  :description "Set working directory"
  :class 'ymacs-transient-ripgrep-directory
  :shortarg "d"
  :argument "@directory="
  :init-value
  (lambda (-obj)
    (oset -obj value (or (ymacs-editor//ripgrep-directory)
                         ymacs-editor-search-directory
                         (ymacs-editor//project-root-or-default)))))

(transient-define-prefix ymacs-editor/ripgrep ()
  "Run ripgrep"
  [["Switches"
    (ymacs-editor-ripgrep-search-hidden)
    (ymacs-editor-ripgrep-search-zip)
    (ymacs-editor-ripgrep-follow-symlink)
    (ymacs-editor-ripgrep-no-ignore)]
   ["Options"
    (ymacs-editor-ripgrep-glob)
    (ymacs-editor-ripgrep-iglob)
    (ymacs-editor-ripgrep-type)
    (ymacs-editor-ripgrep-directory)]]
  ["Action"
   [("RET" "Do search" ymacs-editor//ripgrep-internal)]]
  (interactive)

  (setq ymacs-editor-ripgrep-current-buffer (current-buffer))
  (ymacs-editor//set-ripgrep-input-1 nil)
  (ymacs-editor//set-ripgrep-input-2 nil)

  (if current-prefix-arg
      (transient-setup 'ymacs-editor/ripgrep)
    (ymacs-editor//ripgrep-internal)))

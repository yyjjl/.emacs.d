;; -*- lexical-binding: t -*-

(defsubst ymacs-ivy//display-help-show-keys ()
  (mapconcat
   (lambda (item)
     (when-let (keys (where-is-internal (car item)))
       (format "[%s to %s] "
               (mapconcat
                (lambda (key) (propertize (key-description key) 'face 'hl-line))
                keys
                "/")
               (cdr item))))
   ymacs-ivy-grep-help-commands
   ""))

(defun ymacs-ivy//display-help (-cmd &optional -directory)
  (unless (stringp -cmd)
    (setq -cmd (string-join -cmd " ")))

  (let ((env-string (string-join ymacs-ivy-additional-environment "\n"))
        (key-string (ymacs-ivy//display-help-show-keys)))
    (lv-message
     (format "%s%s(@%s) %s"
             (if (string-empty-p env-string) "" (concat env-string "\n"))
             (if (string-empty-p key-string) "" (concat key-string "\n"))
             (propertize (or -directory default-directory) 'face font-lock-constant-face)
             (propertize -cmd 'face font-lock-doc-face)))))

(defmacro ymacs-ivy//define-switch (name &rest body)
  (declare (indent 1))
  (let ((commands (mapcar #'car body))
        (toggle-fn (intern (format "ymacs-ivy//toggle-between-%s" name)))
        (prompt-fn (intern (format "ymacs-ivy//toggle-between-%s-prompt" name))))
    `(progn
       (add-to-list 'ymacs-ivy-switch-function-list '(,commands . ,toggle-fn))
       (defun ,toggle-fn ()
         ,(format "Toggle %s with the current input."
                  (string-join (--map (format "`%s'" it) commands) ", "))
         (ivy-quit-and-run
           (cl-case (ivy-state-caller ivy-last)
             ,@(--map
                `(,it
                  (,(or (cadr (memq it commands)) ; select next command
                        (car commands))
                   ivy-text))
                commands))))
       (defun ,prompt-fn ()
         (ivy-add-prompt-count
          (cl-case (ivy-state-caller ivy-last)
            ,@(cl-loop
               for command in commands
               collect
               (list
                command
                (concat
                 (string-join (--map (nth (if (eq (nth 0 it) command) 1 2) it) body) "|")
                 ": "))))))

       (dolist (caller ',commands)
         (ivy-set-prompt caller #',prompt-fn)))))

(defun ymacs-ivy/switch ()
  (interactive)
  (let* ((caller (ivy-state-caller ivy-last))
         (toggle-fn (cl-loop
                     for (callers . toggle-fn) in ymacs-ivy-switch-function-list
                     when (memq caller callers)
                     return toggle-fn)))
    (unless toggle-fn
      (user-error "No toggle-function defined"))
    (funcall toggle-fn)))

(defun ymacs-ivy//package-install-transformer (-string)
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

(defun ymacs-ivy//switch-buffer-transformer (-string)
  "Transform STR to more readable format."
  (format "%-60s %s"
          (ivy-switch-buffer-transformer -string)
          (if-let (buffer (get-buffer -string))
              (buffer-local-value 'default-directory buffer)
            "")))

(defun ymacs-ivy//bookmark-transformer (-string)
  "Transform STR to more readable format."
  (let ((bm (bookmark-get-bookmark-record -string)))
    (concat
     (format "%-40s" (propertize -string 'face 'font-lock-string-face))
     (when bm
       (propertize (format "%-10d" (cdr (assoc 'position bm)))
                   'face 'warning))
     (when bm
       (file-name-nondirectory (cdr (assoc 'filename bm)))))))

(defun ymacs-ivy//rg-default-alias ()
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
     ymacs-ivy-rg-type-aliases)))

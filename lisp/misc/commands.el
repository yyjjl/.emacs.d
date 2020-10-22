;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-misc/select-next-error-buffer ()
  (interactive)
  (let* ((error-buffer (next-error-find-buffer))
         (buffers
          (mapcar
           (lambda (buffer)
             (cons (format "%-50s => %s" (buffer-name buffer)
                           (buffer-local-value 'next-error-function buffer))
                   buffer))
           (cl-remove-duplicates
            (cl-remove-if-not
             (lambda (buffer)
               (and (not (eq error-buffer buffer))
                    (or (eq 'TeX-output-mode (buffer-local-value 'major-mode buffer))
                        (and (next-error-buffer-p buffer)
                             (or (eq buffer (current-buffer))
                                 (with-current-buffer buffer
                                   (apply 'derived-mode-p
                                          ymacs-misc-auto-next-error-buffer-derived-modes)))))))
             (buffer-list)))))
         (buffer
          (cdr-safe
           (if (> (length buffers) 1)
               (ymacs//completing-read-simple
                (format "buffer (default: %s): "
                        (when error-buffer (buffer-name error-buffer)))
                buffers)
             (car buffers)))))
    (if buffer
        (progn
          (message "Set `%s' as error buffer" buffer)
          (setq next-error-last-buffer buffer))
      (user-error "Nothing to do"))))

;;;###autoload
(defun ymacs-misc/edit-dir-locals (&optional -directory)
  "Edit or create a .dir-locals.el file of the project."
  (interactive
   (list (let ((root (projectile-project-root)))
           (or (and current-prefix-arg
                    (read-directory-name "Select root" root))
               root))))
  (let ((default-directory -directory))
    (condition-case nil
        (call-interactively #'add-dir-local-variable)
      (quit
       (let ((files (dir-locals--all-files -directory)))
         (cond ((null files)
                (let ((file (expand-file-name ".dir-locals.el" -directory)))
                  (when (yes-or-no-p (format "create file %s" file))
                    (find-file file)
                    (when (not (file-exists-p file))
                      (unwind-protect
                          (projectile-skel-dir-locals)
                        (save-buffer))))))
               ((= (length files) 1)
                (find-file (car files)))
               (t
                (find-file (completing-read "Open file: " files nil t)))))))))

;;;###autoload
(defun ymacs-misc/create-scratch-buffer ()
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        (mode
         (if current-prefix-arg
             (intern
              (ivy-read "Select Mode: " obarray
                        :predicate (lambda (sym)
                                     (and (fboundp sym)
                                          (string-suffix-p "mode" (symbol-name sym))))
                        :require-match t
                        :preselect (symbol-name major-mode)
                        :sort t))
           major-mode))
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (pop-to-buffer (get-buffer-create bufname))
    (funcall mode)))

;;;###autoload
(defun ymacs-misc/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun ymacs-misc/current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let* ((pos (point))
         (text (buffer-substring pos (1+ pos)))
         (faces (-flatten (list (get-char-property pos 'face)
                                (get-char-property 0 'face text)))))
    (message "%s" faces)))

;;;###autoload
(defun ymacs-misc/occur-dwim ()
  (interactive)
  (if (region-active-p)
      (push (buffer-substring-no-properties
             (region-beginning)
             (region-end))
            regexp-history)
    (let ((sym (thing-at-point 'symbol)))
      (when (stringp sym)
        (push (concat "\\_<" (regexp-quote sym) "\\_>") regexp-history))))
  (call-interactively 'occur))

;;;###autoload
(defun ymacs-misc/eval-and-replace (-start -end)
  (interactive "r")
  (let* ((it (buffer-substring -start -end))
         (expr (if (bound-and-true-p mc--executing-command-for-fake-cursor)
                   (read (car read-expression-history))
                 (read--expression "Expression(it): " "(read it)")))
         (value (eval expr `((it . ,it)))))
    (delete-region -start -end)
    (save-excursion
      (goto-char -start)
      (prin1 value (current-buffer))
      (activate-mark 1)
      (goto-char -start))))

(declare-function browse-url-chrome "browse-url")

(defun ymacs-misc//read-search-engine ()
  (assoc-string
   (car (split-string-and-unquote
         (ivy-read
          "Engine: "
          (mapcar
           (lambda (x)
             (format "%-6s %s"
                     (nth 0 x)
                     (propertize (nth 2 x) 'face font-lock-comment-face)))
           ymacs-misc-search-engine-alist)
          :preselect "g"
          :matcher (lambda (re candidates)
                     (when (and (stringp re)
                                (not (string-prefix-p "^" re)))
                       (setq re (concat "^" re)))
                     (ivy--re-filter re candidates))
          :require-match t)))
   ymacs-misc-search-engine-alist))

;;;###autoload
(defun ymacs-misc/search-in-chrome (-engine -keyword)
  (interactive
   (let ((engine (ymacs-misc//read-search-engine)))
     (list (nth 2 engine)
           (read-from-minibuffer (concat (nth 1 engine) ": ")
                                 nil nil 'yamcs-misc-search-history))))
  (unless (featurep 'browse-url)
    (require 'browse-url))
  (browse-url-chrome (format -engine -keyword))
  (run-hooks 'ymacs-misc-after-search-hook))

;;;###autoload
(defun ymacs-misc/toggle-socket-proxy ()
  (interactive)
  (if (eq url-gateway-method 'socks)
      (let ((method (function-get #'ymacs-misc/toggle-socket-proxy 'method)))
        (setq url-gateway-method (or method 'native))
        (message "Use method '%s" url-gateway-method))
    (function-put #'ymacs-misc/toggle-socket-proxy 'method url-gateway-method)
    (setq url-gateway-method 'socks)
    (message "Use socket proxy %s" ymacs-misc-socks-server)))

;;;###autoload
(defun ymacs-misc/delete-http-buffers (-force)
  (interactive "P")
  (require 'dired)
  (let ((buffers
         (--filter
          (and (string-prefix-p " *http " (buffer-name it))
               (not (process-live-p (get-buffer-process it))))
          (buffer-list))))
    (when (and buffers
               (or (not -force)
                   (not (called-interactively-p 'interactive)))
               (dired-mark-pop-up
                " *Deletion*" 'delete (mapcar #'buffer-name buffers) 'yes-or-no-p
                "Delete these buffers "))
      (dolist (buffer buffers)
        (kill-buffer buffer)))))

;;;###autoload
(defun ymacs-misc/rsync-project (-local-path -remote-path -sync-to-remote)
  (interactive
   (list (read-directory-name
          "Local path: "
          ymacs-misc-project-rsync-local-path)
         (read-string
          "Remote path: "
          ymacs-misc-project-rsync-remote-path)
         (completing-read
          "direction:" '("local  -> remote" "remote -> local") nil t)))

  (when (not (file-directory-p -local-path))
    (user-error "'%s' doesn't exist." -local-path))

  (let* ((default-directory -local-path)
         (options (if (not current-prefix-arg)
                      (cl-list* "--dry-run" "--update" ymacs-misc-project-rsync-extra-options)
                    ymacs-misc-project-rsync-extra-options))
         (compile-command
          (format ymacs-misc-project-rsync-command
                  (string-join options " ")
                  (if -sync-to-remote -local-path -remote-path)
                  (if -sync-to-remote -remote-path -local-path))))
    (call-interactively #'compile)))

;;;###autoload
(defun ymacs-misc/toggle-winum-scope ()
  (interactive)
  (setq winum-scope (if (eq winum-scope 'frame-local)
                        'visible
                      'frame-local))
  (message "Current winum scope: %s" (upcase (symbol-name winum-scope)))
  (dolist (frame (frame-list))
    (select-frame frame)
    (winum--update))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode ymacs-misc/view-code-mode
  "View code"
  :init-value nil
  (let ((switch (if ymacs-misc/view-code-mode 1 -1)))
    (cl-loop for (condition . modes) in ymacs-misc-view-code-modes
             when (or (eq condition t)
                      (and (symbolp condition) (symbol-value condition)))
             do (dolist (mode modes)
                  (funcall mode switch)))))

;;; -*- lexical-binding: t; -*-

(defun ymacs-term//load-file-in-repl--default (-args)
  (barf-if-not-visiting-file!)

  (let* ((program (plist-get -args :program))
         (program-args (cl-subst (buffer-file-name)
                                 'the-file
                                 (plist-get -args :program-args)))
         (cmd (plist-get -args :cmd-fmt))
         (callback
          (when cmd
            (let ((file (buffer-file-name)))
              (lambda ()
                (ymacs-term//send-string (format cmd file)))))))
    (ymacs-term//exec-program-in-buffer (concat "Repl: " (buffer-name))
      :program program
      :program-args program-args
      :callback callback)))

;;;###autoload
(defun ymacs-term/toggle-buffer-alive ()
  (interactive)
  (unless (ymacs-term//shell-buffer-p (current-buffer))
    (user-error "not in a shell buffer"))

  (setq mode-line-buffer-identification
        (if ymacs-term-keep-buffer-alive
            (cl-remove "[alive]" mode-line-buffer-identification :test #'equal)
          (append mode-line-buffer-identification '("[alive]"))))

  (setq ymacs-term-keep-buffer-alive (not ymacs-term-keep-buffer-alive)))

;;;###autoloadn
(defun ymacs-term/load-file-in-repl ()
  (interactive)
  (let ((repl (or (alist-get major-mode ymacs-term-repl-alist)
                  (user-error "No entry in `ymacs-term-repl-alist' for %s" major-mode))))
    (if (commandp repl)
        (call-interactively repl)
      (ymacs-term//load-file-in-repl--default repl))))

;;;###autoload
(defun ymacs-term/switch ()
  (interactive)

  (unless (ymacs-popup//term-buffer-p (current-buffer))
    (user-error "Current buffer is not a term buffer"))

  (let ((buffers (mapcar
                  (lambda (buffer)
                    (cons
                     (with-current-buffer buffer
                       (concat (buffer-name)
                               (when ymacs-term-extra-name
                                 (concat ": " ymacs-term-extra-name))))
                     buffer))
                  (cl-remove
                   (current-buffer)
                   (ymacs-popup//get-active-term-buffer-list)))))

    (cl-case (length buffers)
      (0 (user-error "There is only one term buffer"))
      (1 (display-buffer (cdar buffers)))
      (t (pop-to-buffer
          (cdr (completing-read! "Switch to term buffer: " buffers)))))))

;;;###autoload
(defun ymacs-term/next (-create-new)
  (interactive "P")
  (if -create-new
      (ymacs-term/pop-shell-here)
    (ymacs-term//switch-internal -1)))

;;;###autoload
(defun ymacs-term/prev (-create-new)
  (interactive "P")
  (if -create-new
      (ymacs-term/pop-shell-here)
    (ymacs-term//switch-internal 1)))

;;;###autoload
(defun ymacs-term/pop-shell-here ()
  (interactive)
  (when-let (buffer (ymacs-term//create-buffer nil t))
    (display-buffer buffer)
    (with-current-buffer buffer
        (local-set-key [f8] #'ymacs-term/pop-shell))))

;;;###autoload
(defun ymacs-term/pop-shell (&optional -scope -only-shell-p -new-buffer-p)
  (interactive
   (list (cond ((equal current-prefix-arg '(4)) 'project)
               ((equal current-prefix-arg '(16)) 'select)
               (t 'local))
         ;; (equal (prefix-numeric-value current-prefix-arg) 0)
         (not (equal (prefix-numeric-value current-prefix-arg) 0))
         (equal (prefix-numeric-value current-prefix-arg) 16)))

  (if (eq (selected-window) (ymacs-popup//get-term-window))
      (quit-window nil (selected-window))
    (let* ((directory (or (when (eq -scope 'select)
                            (read-directory-name "Shell in: " nil nil :mustmatch))
                          (when (eq -scope 'project)
                            (ymacs-term//get-directory))
                          default-directory))
           (buffer (or (when (and (not -only-shell-p)
                                  (not -new-buffer-p))
                         (if (buffer-live-p ymacs-term--last-buffer)
                             ymacs-term--last-buffer
                           (setq ymacs-term--last-buffer nil)))
                       (when (not -new-buffer-p)
                         (ymacs-term//get-shell-buffer-in-directory directory))
                       (let ((default-directory directory))
                         (ymacs-term//create-buffer nil t)))))
      (display-buffer buffer)
      (with-current-buffer buffer
        (local-set-key [f8] #'ymacs-term/pop-shell)))))


;;;###autoload
(defun ymacs-term/yank-pop (&optional _)
  (interactive "P")
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
      (call-interactively #'consult-yank-pop))))

;;;###autoload
(defun ymacs-term/kill-line ()
  (interactive)
  (kill-new (buffer-substring (point) (line-end-position)))
  (call-interactively #'vterm--self-insert))

;;;###autoload
(defun ymacs-term/line ()
  (interactive)
  (unless (ymacs-term//shell-buffer-p (current-buffer))
    (user-error "Not in `term-mode' buffer"))

  (vterm-copy-mode 1)
  (call-interactively #'consult-line))

;;;###autoload
(defun ymacs-term/toggle-window ()
  (interactive)
  (if-let (window (ymacs-popup//get-term-window))
      (if (eq window (selected-window))
          (delete-window window)
        (select-window window))
    (if-let (buffer (car (ymacs-popup//get-active-term-buffer-list)))
        (display-buffer buffer)
      (call-interactively #'ymacs-term/pop-shell))))

;;;###autoload
(defun ymacs-term/set-extra-name ()
  (interactive)
  (unless (ymacs-term//shell-buffer-p (current-buffer))
    (user-error "Not in a shell buffer"))

  (let ((name (read-from-minibuffer "Extra name: " ymacs-term-extra-name
                                    nil nil
                                    'ymacs-term-extra-name-history)))
    (setq ymacs-term-extra-name (if (string= name "") nil name))))

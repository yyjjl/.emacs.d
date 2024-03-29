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

;;;###autoload
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
         (equal (prefix-numeric-value current-prefix-arg) 0)
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
(defun ymacs-term/conditional-send-raw ()
  (interactive)
  (let ((command (global-key-binding (this-command-keys))))
    ;; When `point' is after last output mark, send raw string
    ;; Otherwise call global binding
    (call-interactively (if (and command (ymacs-term//after-prompt-p))
                            #'term-send-raw
                          command))))

;;;###autoload
(defun ymacs-term/yank-pop (&optional _)
  (interactive "P")
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank)
               (if (eq major-mode 'vterm-mode)
                   (lambda (str) (vterm-send-string str t))
                 (lambda (str) (term-send-raw-string str)))))
      (call-interactively #'consult-yank-pop))))

;;;###autoload
(defun ymacs-term/kill-line ()
  (interactive)
  (if (eq major-mode 'term-mode)
      (if (ymacs-term//after-prompt-p)
          (progn
            (kill-new (buffer-substring (point) (line-end-position)))
            (term-send-raw-string "\C-k"))
        (call-interactively #'kill-line))
    (kill-new (buffer-substring (point) (line-end-position)))
    (call-interactively #'vterm--self-insert)))

;;;###autoload
(defun ymacs-term/send-backward-word ()
  (interactive)
  (if (ymacs-term//after-prompt-p)
      (term-send-raw-string "\eb")
    (call-interactively #'backward-word)))

;;;###autoload
(defun ymacs-term/send-forward-word ()
  (interactive)
  (if (ymacs-term//after-prompt-p)
      (term-send-raw-string "\ef")
    (call-interactively #'forward-word)))

;;;###autoload
(defun ymacs-term/line ()
  (interactive)
  (unless (ymacs-term//shell-buffer-p (current-buffer))
    (user-error "Not in `term-mode' buffer"))

  (if (eq major-mode 'term-mode)
      (term-line-mode)
    (vterm-copy-mode 1))

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

(defmacro ymacs-term//define-send-key (name key doc)
  (declare (indent 2))
  `(defun ,(intern (format "ymacs-term/send-%s" name)) ()
     ,doc
     (interactive)
     (term-send-raw-string ,key)))

(ymacs-term//define-send-key esc "\e" "Send ESC in term mode.")
(ymacs-term//define-send-key return "\C-m" "Send Return in term mode.")
(ymacs-term//define-send-key backward-kill-word "\C-w" "Backward kill word in term mode.")
(ymacs-term//define-send-key forward-kill-word "\ed" "Forward kill word in term mode.")
(ymacs-term//define-send-key reverse-search-history "\C-r" "Search history reverse.")
(ymacs-term//define-send-key delete-word "\ed" "Delete word in term mode")
(ymacs-term//define-send-key M-x "\ex" "Type M-x in term-mode.")
(ymacs-term//define-send-key up "\ep" "Type M-p in term-mode.")
(ymacs-term//define-send-key down "\en" "Type M-n in term-mode.")
(ymacs-term//define-send-key M-l "\el" "Type M-l in term-mode.")
(ymacs-term//define-send-key M-c "\ec" "Type M-c in term-mode.")
(ymacs-term//define-send-key M-u "\eu" "Type M-u in term-mode.")
(ymacs-term//define-send-key undo "\C-_" "Type undo in term-mode.")

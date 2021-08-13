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
(defun ymacs-term/switch-back (-no-quit-p)
  (interactive "P")

  (unless (buffer-live-p ymacs-term--parent-buffer)
    (user-error "No parent buffer or it was killed !!!"))

  (when (eq (selected-window)
            (ymacs-popup//get-term-window))
    (pop-to-buffer ymacs-term--parent-buffer)

    (unless -no-quit-p
      (delete-window (ymacs-popup//get-term-window)))))

(defun ymacs-term/switch-back-no-quit ()
  (interactive)
  (ymacs-term/switch-back t))

;;;###autoload
(defun ymacs-term/pop-shell-here ()
  (interactive)
  (when-let ((parent-buffer (or ymacs-term--parent-buffer (current-buffer)))
             (buffer (ymacs-term//create-buffer nil t)))
    (with-current-buffer buffer
      (local-set-key [f8] #'ymacs-term/switch-back)
      (local-set-key (kbd "C-c C-z") #'ymacs-term/switch-back-no-quit)
      (local-set-key (kbd "C-c z") #'ymacs-term/switch-back-no-quit)
      (setq ymacs-term--parent-buffer parent-buffer))
    (display-buffer buffer)))

;;;###autoload
(defun ymacs-term/pop-shell (&optional -arg)
  "Popup to a term buffer.
-ARG = 0: create a term buffer in current window
-ARG >= 16: try to find a old term buffer by given directory
else: try to find a old term buffer and pop to it"
  (interactive "p")
  (when-let* ((buffer (ymacs-term//pop-shell-get-buffer -arg))
              (parent-buffer (current-buffer)))
    (if (= -arg 0)
        (switch-to-buffer buffer)
      (with-current-buffer buffer
        (local-set-key [f8] #'ymacs-term/switch-back)
        (local-set-key (kbd "C-c C-z") #'ymacs-term/switch-back-no-quit)
        (local-set-key (kbd "C-c z") #'ymacs-term/switch-back-no-quit)
        (setq ymacs-term--parent-buffer parent-buffer)))
    (display-buffer buffer)))

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

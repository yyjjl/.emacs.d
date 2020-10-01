;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-term/switch ()
  (interactive)

  (unless (ymacs-term//term-buffer-p (current-buffer))
    (user-error "Current buffer is not a term buffer"))

  (let ((buffers
         (--map
          (let ((name (buffer-local-value 'ymacs-term-extra-name it)))
            (cons (concat (buffer-name it)
                          (when name
                            (concat ": " name)))
                  it))
          (ymacs-term//buffers))))
    (if (<= (length buffers) 1)
        (user-error "There is only one term buffer")

      (ivy-read "Switch to term buffer: " buffers
                :require-match t
                :action (lambda (candidate)
                          (let ((window (selected-window)))
                            (set-window-dedicated-p window nil)
                            (unwind-protect
                                (switch-to-buffer (cdr candidate))
                              (set-window-dedicated-p window t))))))))

;;;###autoload
(defun ymacs-term/next (-create-new)
  (interactive "P")
  (if -create-new
      (ymacs-term/pop-shell-here)
    (ymacs-term//switch-internal 1)))

;;;###autoload
(defun ymacs-term/prev (-create-new)
  (interactive "P")
  (if -create-new
      (ymacs-term/pop-shell-here)
    (ymacs-term//switch-internal -1)))

;;;###autoload
(defun ymacs-term/switch-back (-no-quit-p)
  (interactive "P")
  (unless (buffer-live-p ymacs-term--parent-buffer)
    (user-error "No parent buffer or it was killed !!!"))
  (when (eq (ymacs-term//get-window) (selected-window))
    (pop-to-buffer ymacs-term--parent-buffer)
    (unless -no-quit-p
      (delete-window (ymacs-term//get-window)))))

;;;###autoload
(defun ymacs-term/pop-shell-here ()
  (interactive)
  (when-let ((parent-buffer (or ymacs-term--parent-buffer (current-buffer)))
             (buffer (ymacs-term//create-buffer nil t)))
    (with-current-buffer buffer
      (setq ymacs-term-autokill-p 'shell)
      (local-set-key [f8] #'ymacs-term/switch-back)
      (local-set-key (kbd "C-c C-z") (lambda! (ymacs-term/switch-back t)))
      (setq ymacs-term--parent-buffer parent-buffer))
    (ymacs-term//display-buffer buffer)))

;;;###autoload
(defun ymacs-term/pop-shell (&optional -arg)
  "Popup to a term buffer.
-ARG = 0: create a term buffer in current window
-ARG >= 16: try to find a old term buffer by given directory
else: try to find a old term buffer and pop to it"
  (interactive "p")
  (when-let* ((buffer (ymacs-term//pop-shell-get-buffer -arg))
              (parent-buffer (current-buffer)))
    (with-current-buffer buffer
      (setq ymacs-term-autokill-p 'shell))
    (if (= -arg 0)
        (switch-to-buffer buffer)
      (with-current-buffer buffer
        (local-set-key [f8] #'ymacs-term/switch-back)
        (local-set-key (kbd "C-c C-z") (lambda! (ymacs-term/switch-back t)))
        (setq ymacs-term--parent-buffer parent-buffer))
      (ymacs-term//display-buffer buffer))))

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
      (call-interactively #'counsel-yank-pop))))

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
(defun ymacs-term/swiper ()
  (interactive)
  (unless (ymacs-term//term-buffer-p (current-buffer))
    (user-error "Not in `term-mode' buffer"))

  (if (eq major-mode 'term-mode)
      (term-line-mode)
    (vterm-copy-mode 1))

  (call-interactively 'ymacs/swiper))

;;;###autoload
(defun ymacs-term/set-extra-name ()
  (interactive)
  (unless (ymacs-term//term-buffer-p (current-buffer))
    (user-error "Not in a term buffer"))

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
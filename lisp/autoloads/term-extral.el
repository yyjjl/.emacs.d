;; Kill the buffer when terminal is exited
;;;###autoload
(defvar term-default-directory-function-list '(projectile-project-root))
;;;###autoload
(defvar term-default-environment-function-list nil)

(defun term//after-prompt? ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (mark (and proc (process-mark proc)))
         (pos (point)))
    (and mark (>= pos mark))))

(defun term//eval-function-list (variable)
  (let ((functions (symbol-value variable))
        value)
    (while (and functions
                (not (setq value (ignore-errors
                                   (funcall (pop functions)))))))
    value))

(defun term//default-sentinel (proc msg)
  (term-sentinel proc msg)
  (when (memq (process-status proc) '(signal exit))
    (with-current-buffer (process-buffer proc)
      (let ((buffer-read-only nil))
        (insert (propertize "Press `Ctrl-D' or `q' to kill this buffer. "
                            'font-lock-face 'font-lock-comment-face)))
      (setq buffer-read-only t)
      (local-set-key (kbd "C-d") (lambda! (kill-buffer)))
      (local-set-key (kbd "q") (lambda! (kill-buffer))))))

(defun term//get-buffer-name ($fmt)
  (let ((index 1) name)
    (setq name (format $fmt index))
    (while (buffer-live-p (get-buffer name))
      (setq name (format $fmt index))
      (setq index (1+ index)))
    name))

(defun term//get-ssh-info ($arg)
  (let ((user (file-remote-p default-directory 'user))
        (host (file-remote-p default-directory 'host))
        (method (file-remote-p default-directory 'method))
        port)
    (when (and method (not (member method '("sshx" "ssh"))))
      (error "Can not open ssh connection for method `%s'" method))
    (when (or (= $arg 4) (not user))
      (setq user (read-string "User: " user)))
    (when (or (= $arg 4) (not host))
      (setq user (read-string "Host: " host)))
    (when (= $arg 4)
      (setq port (read-string "Port: " "22")))
    (list user host port (>= $arg 16))))

;;;###autoload
(defun term/exec-program ($program $args &optional $name $sentinel)
  (unless (featurep 'term)
    (require 'term))
  (unless $sentinel
    (setq $sentinel 'term//default-sentinel))
  (unless $name
    (setq $name (term//get-buffer-name
                 (concat "*" (file-name-nondirectory $program) "<%s>*"))))
  (let ((buf (generate-new-buffer $name))
        (parent-buf (current-buffer)))
    (with-current-buffer buf
      (term-mode)
      (setq term--parent-buffer parent-buf)
      (term-exec buf $name $program nil $args)
      (let ((proc (get-buffer-process buf)))
        (if (and proc (eq 'run (process-status proc)))
            (set-process-sentinel proc $sentinel)
          (error "Failed to invoke command")))
      (term-char-mode))
    buf))

;;;###autoload
(defun term/switch-back ()
  (interactive)
  (if (and term--parent-buffer (buffer-live-p term--parent-buffer))
      (progn
        (when (eq (term//get-popup-window) (selected-window))
          (pop-to-buffer term--parent-buffer)
          (delete-window (term//get-popup-window))))
    (message "No parent buffer or it was killed !!!")))

;;;###autoload
(defun term/switch-back-no-quit ()
  (interactive)
  (if (and term--parent-buffer (buffer-live-p term--parent-buffer))
      (pop-to-buffer term--parent-buffer)
    (message "No parent buffer or it was killed !!!")))

;;;###autoload
(defun term/ssh ($user $host &optional $port $force)
  (interactive (term//get-ssh-info (or (car-safe current-prefix-arg) 0)))
  (let ((args (list (format "%s@%s" $user $host)
                    (format "-p %s" (or $port 22)))))
    (or (and (not $force)
             (car (--filter (with-current-buffer it
                              (and (eq major-mode 'term-mode)
                                   (equal args term--ssh-info)))
                            (buffer-list))))
        (let ((buffer (term/exec-program "ssh" args)))
          (with-current-buffer buffer
            (setq term--ssh-info args))
          buffer))))

(defun term//local-shell ($directory &optional $force)
  "If there is a term buffer whose default-directory is $DIRECTORY,
return that buffer. Otherwise create a new term buffer.

If $FORCE is non-nil create a new term buffer directly."
  (or (and (not $force)
           (car (--filter (with-current-buffer it
                            (and (eq major-mode 'term-mode)
                                 (directory-equal? $directory
                                                   default-directory)))
                          (buffer-list))))
      (let* ((default-directory $directory)
             (buffer (with-temp-env! (term//eval-function-list
                                      'term-default-environment-function-list)
                       (multi-term-get-buffer))))
        (setq multi-term-buffer-list
              (nconc multi-term-buffer-list (list buffer)))
        (with-current-buffer buffer
          ;; Internal handle for `multi-term' buffer.
          (multi-term-internal))
        buffer)))

;;;###autoload
(defun term//pop-to-buffer (buffer)
  (let ((popup-window (term//get-popup-window)))
    (if (and (window-live-p popup-window)
             (eq 'term-mode
                 (buffer-local-value 'major-mode
                                     (window-buffer popup-window))))
        ;; Reuse window
        (progn
          (select-window popup-window)
          (set-window-dedicated-p popup-window nil)
          (set-window-buffer popup-window buffer))
      (pop-to-buffer buffer)
      (term//set-popup-window (get-buffer-window buffer)))))

;;;###autoload
(defun term/pop-shell (&optional $arg)
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive "p")
  (unless (featurep 'multi-term)
    (require 'multi-term))
  (unless (memq major-mode '(eshell-mode term-mode shell-mode))
    (let ((buffer (if (file-remote-p default-directory)
                      (apply #'term/ssh (term//get-ssh-info $arg))
                    (term//local-shell
                     (or (and (equal $arg 4)
                              (term//eval-function-list
                               'term-default-directory-function-list))
                         default-directory)
                     (equal $arg 16))))
          (parent-buffer (current-buffer)))
      (when buffer
        (with-current-buffer buffer
          (local-set-key [f8] #'term/switch-back)
          (local-set-key (kbd "C-c C-z") #'term/switch-back-no-quit)
          (setq term--parent-buffer parent-buffer))
        (term//pop-to-buffer buffer)))))

;;;###autoload
(defun term/conditional-send-raw ()
  (interactive)
  (let ((command (global-key-binding (this-command-keys))))
    ;; When `point' is after last output mark, send raw string
    ;; Otherwise call global binding
    (call-interactively (if (and command (term//after-prompt?))
                            #'term-send-raw
                          command))))

;;;###autoload
(defun term-send-backward-word ()
  (interactive)
  (if (term//after-prompt?)
      (term-send-raw-string "\eb")
    (call-interactively #'backward-word)))

;;;###autoload
(defun term-send-forward-word ()
  (interactive)
  (if (term//after-prompt?)
      (term-send-raw-string "\ef")
    (call-interactively #'forward-word)))

;;;###autoload
(defun term/previous-line ()
  (interactive)
  ;; Fix for Emacs 26
  (setq term-goto-process-mark nil)
  (forward-line -1))

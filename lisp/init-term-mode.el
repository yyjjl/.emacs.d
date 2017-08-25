;; Add below code to .zshrc to make term-mode track directory changes
;;   if [ -n "$INSIDE_EMACS" ];then
;;       chpwd() {print -P "\033AnSiTc %d"}
;;   fi

;; Kill the buffer when terminal is exited
(defun term|wrap-sentinel (&optional _fn)
  (lexical-let ((fn _fn)
                (kill-window-p (= 1 (length (window-list)))))
    (lambda (proc msg)
      (if (memq (process-status proc) '(signal exit))
          (let* ((buffer (process-buffer proc))
                 (window (get-buffer-window buffer)))
            (and fn (funcall fn proc msg))
            (when (and (window-live-p window)
                       (> (length (window-list)) 1)
                       kill-window-p)
              (delete-window window))
            (kill-buffer buffer))
        (and fn (funcall fn proc msg))))))

(defun term|exec-sentinel (proc msg)
  (and (memq (process-status proc) '(signal exit))
       (let ((buf (and proc (process-buffer proc))))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq buffer-read-only t)
             (local-set-key (kbd "q") 'kill-this-buffer)))))
  (term-sentinel proc msg))

(defun term|exec-program (program args &optional sentinel)
  (unless (featurep 'term)
    (require 'term))
  (unless sentinel
    (setq sentinel #'term|exec-sentinel))
  (let ((buf (generate-new-buffer
              (concat "*" (file-name-nondirectory program) "*")))
        (parent-buf (current-buffer)))
    (with-current-buffer buf
      (term-mode)
      (setq term|parent-buffer parent-buf)
      (term-exec buf program program nil args)
      (let ((proc (get-buffer-process buf)))
        (if (and proc (eq 'run (process-status proc)))
            (set-process-sentinel proc sentinel)
          (error "Failed to invoke visual command")))
      (term-char-mode))
    buf))

(defhook term|shell-setup (shell-mode-hook)
  (let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
    (when proc
      (set-process-sentinel proc
                            (term|wrap-sentinel (process-sentinel proc))))))

(defhook term|utf8-setup (term-exec-hook)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(defun term|last-buffer (buffers mode &optional dir)
  "Return most recently used term buffer."
  (when buffers
    (let* ((buf (car buffers))
           (info (with-current-buffer buf
                   (cons major-mode default-directory))))
      (if (and (eq mode (car info))
               (directory-equal-p (or dir default-directory) (cdr info)))
          buf
        (term|last-buffer (cdr buffers) mode dir)))))

(defun term|get-buffer-name (fmt)
  (let ((index 1) name)
    (setq name (format fmt index))
    (while (buffer-live-p (get-buffer name))
      (setq name (format fmt index))
      (setq index (1+ index)))
    name))

(defvar-local term|parent-buffer nil)
(defun term|switch-back ()
  (interactive)
  (if (and term|parent-buffer (buffer-live-p term|parent-buffer))
      (pop-to-buffer term|parent-buffer)
    (message "No parent buffer or it was killed !!!")))

(defun term|remote-shell (&optional force)
  "Switch to remote shell"
  (let ((buf (or (and (not force) (term|last-buffer (buffer-list) 'shell-mode))
                 (get-buffer-create (term|get-buffer-name "*shell-%d*")))))
    (when buf
      (with-current-buffer buf
        (unless (eq major-mode 'shell-mode)
          (let ((explicit-shell-file-name "/bin/bash"))
            (shell buf)))))
    buf))

(defun term|local-shell (&optional dir force)
  (unless (featurep 'multi-term)
    (require 'multi-term nil t))
  (let ((buf (and (not force)
                  (term|last-buffer (buffer-list) 'term-mode dir))))
    (unless buf
      (if dir
          (let ((default-directory dir))
            (setq buf (multi-term-get-buffer)))
        (setq buf (multi-term-get-buffer)))
      (setq multi-term-buffer-list
            (nconc multi-term-buffer-list (list buf)))
      (with-current-buffer buf
        ;; Internal handle for `multi-term' buffer.
        (multi-term-internal)
        (let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
          (when proc (set-process-sentinel proc (term|wrap-sentinel))))))
    buf))

(defun term|pop-shell (&optional arg)
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive "p")
  (unless (featurep 'multi-term)
    (require 'multi-term))
  (unless (memq major-mode '(eshell-mode term-mode shell-mode))
    (let ((buf (if (not (file-remote-p default-directory))
                   (funcall (if term|use-eshell-p
                                #'term|local-eshell
                              #'term|local-shell)
                            (or (and (not (equal arg 4)) default-directory)
                                (and (bound-and-true-p cpp|cmake-ide-enabled)
                                     cmake-ide-build-dir)
                                (ignore-errors (projectile-project-root)))
                            (equal arg 16))
                 (term|remote-shell (equal arg 16))))
          (parent-buf (current-buffer)))
      (when buf
        (with-current-buffer buf
          (local-set-key [f8] #'term|switch-back)
          (setq term|parent-buffer parent-buf))
        (pop-to-buffer buf)))))

(with-eval-after-load 'multi-term
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)

  (setq multi-term-program (or term|zsh-path term|bash-path))
  (setq term-unbind-key-list '("C-x" "<ESC>" "C-y" "C-h" "C-c" "C-g"))
  (setq term-bind-key-alist
        (append term-bind-key-alist
                '(("C-c C-n" . multi-term)
                  ("C-s" . swiper)
                  ("M-]" . multi-term-next)
                  ("M-[" . multi-term-prev))))
  (setq multi-term-dedicated-close-back-to-open-buffer-p t))

(global-set-key [f8] 'term|pop-shell)

(provide 'init-term-mode)

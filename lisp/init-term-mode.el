;; kill the buffer when terminal is exited
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

(defhook term|shell-setup (shell-mode-hook)
  (let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
    (when proc
      (set-process-sentinel proc
                            (term|wrap-sentinel (process-sentinel proc))))))

(defhook term|term-setup (term-mode-hook)
  (dirtrack-mode 1)
  (setq dirtrack-list '("^#.*?in \\(.*?\\) \\[[:0-9]*\\]" 1 nil)))

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

(defun term|remote-shell ()
  "Switch to remote shell"
  (interactive)
  (let ((buf (or (term|last-buffer (buffer-list) 'shell-mode)
                 (get-buffer-create (term|get-buffer-name "*shell-%d*")))))
    (when buf
      (pop-to-buffer buf)
      (set-buffer buf)
      (unless (eq major-mode 'shell-mode)
        (let ((explicit-shell-file-name "/bin/bash"))
          (shell buf))))))

(defun term|local-shell (&optional dir)
  (unless (featurep 'multi-term)
    (require 'multi-term nil t))
  (let ((buf (term|last-buffer (buffer-list) 'term-mode dir)))
    (unless buf
      (if dir
          (let ((default-directory dir))
            (setq buf (multi-term-get-buffer)))
        (setq buf (multi-term-get-buffer)))
      (setq multi-term-buffer-list
            (nconc multi-term-buffer-list (list buf)))
      (set-buffer buf)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      (let ((proc (ignore-errors (get-buffer-process (current-buffer)))))
        (when proc (set-process-sentinel proc (term|wrap-sentinel)))))
    buf))

;; `eshell' setup
(defun term|eshell-autoclose ()
  (let ((window (get-buffer-window)))
    (when (and (window-live-p window)
               (> (length (window-list)) 1))
      (delete-window window))))

(defun term|local-eshell (&optional dir)
  (unless dir
    (setq dir default-directory))
  (let ((buf (or (term|last-buffer (buffer-list) 'eshell-mode dir)
                 (get-buffer-create (term|get-buffer-name "*eshell-%d*")))))
    (with-current-buffer buf
      (cd dir)
      (unless (eq 'eshell-mode major-mode)
        (eshell-mode)
        (add-hook 'kill-buffer-hook #'term|eshell-autoclose nil :local)))
    buf))


(defvar term|use-eshell-p t)

(defun term|pop-shell (&optional arg)
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive "P")
  (if (not (file-remote-p default-directory))
      (unless (memq major-mode '(term-mode eshell-mode))
        (pop-to-buffer
         (funcall (if term|use-eshell-p
                      #'term|local-eshell
                    #'term|local-shell)
                  (or (and arg default-directory)
                      (and (bound-and-true-p cpp|cmake-ide-enabled)
                           cmake-ide-build-dir)
                      (ignore-errors (projectile-project-root))))))
    (term|remote-shell)))

(with-eval-after-load 'multi-term
  (setq multi-term-program "/bin/zsh")
  (setq term-unbind-key-list '("C-x" "<ESC>" "C-y" "C-h" "C-c"))
  (setq term-bind-key-alist
        (append term-bind-key-alist
                '(("C-c C-n" . multi-term)
                  ("C-s" . swiper)
                  ("M-]" . multi-term-next)
                  ("M-[" . multi-term-prev))))
  (setq multi-term-dedicated-close-back-to-open-buffer-p t))

(global-set-key [f8] 'term|pop-shell)

(provide 'init-term-mode)

;; kill the buffer when terminal is exited
(defun autoclose-term-buffer (fn proc msg)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        (funcall fn proc msg)
        (kill-buffer buffer))
    (funcall fn proc msg)))

(defun autoclose-shell-buffer (fn proc msg)
  (if (and (eq major-mode 'shell-mode) (memq (process-status proc) '(signal exit)))
      (let ((buffer (process-buffer proc)))
        (funcall fn proc msg)
        (kill-buffer buffer))
    (funcall fn proc msg)))
(advice-add 'term-sentinel :around #'autoclose-term-buffer)
(advice-add 'tramp-process-sentinel :around #'autoclose-term-buffer)

;; utf8
(defun term-mode-utf8-setup ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'term-mode-utf8-setup)

(defun directory-equal-p (d1 d2)
  (equal (expand-file-name (concat d1 "/"))
         (expand-file-name (concat d2 "/"))))


(defun last-term-buffer (buffers mode &optional dir)
  "Return most recently used term buffer."
  (when buffers
    (let* ((buf (car buffers))
           (info (with-current-buffer buf
                   (cons major-mode default-directory))))
      (if (and (eq mode (car info))
              (directory-equal-p (or dir default-directory) (cdr info)))
          buf
        (last-term-buffer (cdr buffers) mode dir)))))

(defun get-first-buffer-name (fmt)
  (let ((index 1) name)
    (setq name (format fmt index))
    (while (buffer-live-p (get-buffer name))
      (setq index (1+ index)))
    name))

(defun get-remote-shell ()
  "Switch to remote shell"
  (interactive)
  (let ((buf   (or (last-term-buffer (buffer-list) 'shell-mode)
                   (get-buffer-create (get-first-buffer-name "*shell%d*")))))
    (when buf
      (pop-to-buffer buf)
      (set-buffer buf)
      (unless (eq major-mode 'shell-mode)
        (let ((explicit-shell-file-name "/bin/bash"))
          (shell buf))))))

(defun get-term (&optional dir)
  (unless (featurep 'multi-term)
    (require 'multi-term nil t))
  (let ((buf (last-term-buffer (buffer-list) 'term-mode dir)))
    (unless buf
      (if dir
          (let ((default-directory dir))
            (setq buf (multi-term-get-buffer)))
        (setq buf (multi-term-get-buffer)))
      (setq multi-term-buffer-list
            (nconc multi-term-buffer-list (list buf)))
      (set-buffer buf)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal))
    buf))

(defun get-term-or-shell (&optional arg)
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive "P")
  (if (not (file-remote-p default-directory))
      (unless (eq major-mode 'term-mode)
        (pop-to-buffer
         (get-term (or (and arg default-directory)
                       (and (bound-and-true-p cmake-ide-enabled)
                            cmake-ide-build-dir)
                       (ignore-errors (projectile-project-root))))))
    (get-remote-shell)))

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

(defun term-mode-setup ()
  (dirtrack-mode 1)
  (setq dirtrack-list '("^#.*?in \\(.*?\\) \\[[:0-9]*\\]" 1 nil)))

(add-hook 'term-mode-hook 'term-mode-setup)

(global-set-key [f8] 'get-term-or-shell)

(provide 'init-term-mode)

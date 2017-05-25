;; kill the buffer when terminal is exited
(defun autoclose-term-buffer (fn proc msg)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        (funcall fn proc msg)
        (kill-buffer buffer))
    (funcall fn proc msg)))
(advice-add 'term-sentinel :around #'autoclose-term-buffer)

;; utf8
(defun term-mode-utf8-setup ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'term-mode-utf8-setup)


(defun last-term-buffer (buffers mode)
  "Return most recently used term buffer."
  (when buffers
    (let* ((buf (car buffers))
           (info (with-current-buffer buf
                   (cons major-mode default-directory))))
      (if (and (eq mode (car info)) (equal default-directory
                                           (cdr info)))
          buf
        (last-term-buffer (cdr buffers) mode)))))

(defun get-first-buffer-name (fmt)
  (let ((index 1) name)
    (setq name (format fmt index))
    (while (buffer-live-p (get-buffer name))
      (setq index (1+ index)))
    name))

(defun get-eshell (name)
  "Switch to eshell"
  (let ((path  (s-join ":" (tramp-get-remote-path
                                 (tramp-dissect-file-name name))))
        (buf (last-term-buffer (buffer-list) 'eshell-mode)))
    (unless buf
      (setq buf (get-buffer-create (get-first-buffer-name "*eshell*<%d>*")))
      (set-buffer buf)
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode)
        (setq eshell-path-env path)))
    buf))

(defun get-term ()
  (unless (featurep 'multi-term)
    (require 'multi-term nil t))
  (let ((buf (last-term-buffer (buffer-list) 'term-mode)))
    (unless buf
      (setq buf (multi-term-get-buffer current-prefix-arg))
      (setq multi-term-buffer-list
            (nconc multi-term-buffer-list (list buf)))
      (set-buffer buf)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal))
    buf))

(defun get-term-or-eshell ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (if (and default-directory (file-remote-p default-directory))
      (pop-to-buffer (get-eshell (buffer-file-name)))
    (pop-to-buffer (get-term))))

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

(with-eval-after-load 'esh-opt
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))
(with-eval-after-load 'esh-module
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(global-set-key [f8] 'get-term-or-eshell)

(provide 'init-term-mode)

(defvar! term-zsh-path (executable-find "zsh")
  "Zsh path")
(defvar! term-bash-path (executable-find "bash")
  "Bash path")

(eval-when-compile
  (require 'dash))

;; Add below code to .zshrc to make term-mode track directory changes
;;   if [ -n "$INSIDE_EMACS" ];then
;;       chpwd() {print -P "\033AnSiTc %d"}
;;   fi

;; Kill the buffer when terminal is exited
(defun term/wrap-sentinel (&optional _fn)
  (lexical-let ((fn _fn))
    (lambda ($proc $msg)
      (and fn (funcall fn $proc $msg))
      (when (memq (process-status $proc) '(signal exit))
          (let ((buffer (process-buffer $proc)))
            (kill-buffer buffer))))))

(defun term/exec-program ($program $args &optional $name $sentinel)
  (unless (featurep 'term)
    (require 'term))
  (unless $sentinel
    (setq $sentinel (term/wrap-sentinel #'term-sentinel)))
  (unless $name
    (setq $name (term/get-buffer-name
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

(defun term/get-buffer-name (fmt)
  (let ((index 1) name)
    (setq name (format fmt index))
    (while (buffer-live-p (get-buffer name))
      (setq name (format fmt index))
      (setq index (1+ index)))
    name))

(defvar-local term--parent-buffer nil)
(defun term/switch-back ()
  (interactive)
  (if (and term--parent-buffer (buffer-live-p term--parent-buffer))
      (pop-to-buffer term--parent-buffer)
    (message "No parent buffer or it was killed !!!")))

(defun term/get-ssh-info ($arg)
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

(defvar-local term--ssh-info nil)
(defun term/ssh ($user $host &optional $port $force)
  (interactive (term/get-ssh-info (or (car-safe current-prefix-arg) 0)))
  (let ((args (list (format "%s@%s" $user $host)
                    (format "-p %s" (or $port 22)))))
    (let ((buf (or  (and (not $force)
                         (car (--filter (with-current-buffer it
                                          (and (eq major-mode 'term-mode)
                                               (equal args term--ssh-info)))
                                        (buffer-list))))
                    (term/exec-program "ssh" args))))
      (with-current-buffer buf
        (setq term--ssh-info args))
      buf)))

(defun term/local-shell (&optional $dir $force)
  (unless (featurep 'multi-term)
    (require 'multi-term nil t))
  (unless $dir
    (setq $dir default-directory))
  (let ((buf (or (and (not $force)
                      (car (--filter (with-current-buffer it
                                       (and (eq major-mode 'term-mode)
                                            (directory-equal? $dir default-directory)))
                                     (buffer-list))))
                 (let ((default-directory $dir))
                   (multi-term-get-buffer)))))
    (setq multi-term-buffer-list
          (nconc multi-term-buffer-list (list buf)))
    (with-current-buffer buf
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal))
    buf))

(defun term/pop-shell (&optional $arg)
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive "p")
  (unless (featurep 'multi-term)
    (require 'multi-term))
  (unless (memq major-mode '(eshell-mode term-mode shell-mode))
    (let ((buf (if (not (file-remote-p default-directory))
                   (term/local-shell
                    (or (and (not (equal $arg 4)) default-directory)
                        (and (bound-and-true-p cpp-cmake-ide-enabled)
                             (boundp 'cmake-ide-build-dir)
                             cmake-ide-build-dir)
                        (ignore-errors (projectile-project-root)))
                    (equal $arg 16))
                 (apply #'term/ssh (term/get-ssh-info $arg))))
          (parent-buf (current-buffer)))
      (when buf
        (with-current-buffer buf
          (local-set-key [f8] #'term/switch-back)
          (setq term--parent-buffer parent-buf))
        (pop-to-buffer buf)))))

(with-eval-after-load 'multi-term
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (setq multi-term-scroll-to-bottom-on-output t)
  (setq multi-term-program (or term-zsh-path term-bash-path))
  (setq term-bind-key-alist
        (append term-bind-key-alist
                '(("C-s" . swiper/dispatch)
                  ("M-]" . multi-term-next)
                  ("M-[" . multi-term-prev)
                  ("C-g" . keyboard-quit))))
  (setq multi-term-dedicated-close-back-to-open-buffer-p t))

(define-hook! term|utf8-setup (term-exec-hook)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(define-hook! term|autoclose-buffer (comint-mode-hook)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc
                            (term/wrap-sentinel (process-sentinel proc))))))

(global-set-key [f8] 'term/pop-shell)

(provide 'core-term)

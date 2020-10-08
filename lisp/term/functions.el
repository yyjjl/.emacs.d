;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'term))

(autoload 'tramp-parse-sconfig "tramp" nil nil)

(defsubst ymacs-term//parse-sconfig ()
  (remove nil (mapcar #'cadr (tramp-parse-sconfig "~/.ssh/config"))))

(defun ymacs-term//get-host ()
  (let* ((user (file-remote-p default-directory 'user))
         (host (file-remote-p default-directory 'host)))
    (completing-read
     "[user[#port]@]host: " (ymacs-term//parse-sconfig)
     nil                                ; predicate
     nil                                ; require-match
     (if user (concat user "@" host) host))))

(defsubst ymacs-term//get-window ()
  (when-let (window (frame-parameter nil 'ymacs-term-window))
    (and (window-live-p window) window)))

(defsubst ymacs-term//set-window (-popup-window)
  (set-window-dedicated-p -popup-window t)
  (set-frame-parameter nil 'ymacs-term-window -popup-window))

(defsubst ymacs-term//get-buffer-name (-fmt)
  (let* ((index 1)
         (name (format -fmt index)))
    (while (buffer-live-p (get-buffer name))
      (setq name (format -fmt index))
      (setq index (1+ index)))
    name))

(defun ymacs-term//setup-keybindings ()
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (dolist (unbind-key ymacs-term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `ymacs-term-bind-key-alist'
    (dolist (element ymacs-term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defsubst ymacs-term//term-buffer-p (-buffer)
  (and (buffer-live-p -buffer)
       (not (string-prefix-p " " (buffer-name -buffer)))
       (with-current-buffer -buffer
         (derived-mode-p 'term-mode 'shell-mode 'eshell-mode 'vterm-mode))))

(defsubst ymacs-term//window-display-term-buffer-p (window)
  (and (window-live-p window)
       (ymacs-term//term-buffer-p (window-buffer window))))

(defun ymacs-term//display-buffer (-buffer)
  (if-let (popup-window
           (-first #'ymacs-term//window-display-term-buffer-p
                   (cons (ymacs-term//get-window)
                         (window-list))))
      ;; Reuse window
      (progn
        (select-window popup-window)
        (set-window-dedicated-p popup-window nil)
        (set-window-buffer popup-window -buffer))

    (pop-to-buffer -buffer)
    (ymacs-term//set-window (get-buffer-window -buffer))))

(defun ymacs-term//buffers ()
  (--sort
   (string< (buffer-name it) (buffer-name other))
   (--filter (ymacs-term//term-buffer-p it) (buffer-list))))

(defun ymacs-term//switch-internal (-n &optional -ignore-self)
  (let ((buffers (ymacs-term//buffers)))
    (when -ignore-self
      (setq buffers (remove (current-buffer) buffers)))

    (when buffers
      (let ((size (length buffers))
            (index (cl-position (current-buffer) buffers))
            (window (selected-window)))
        (set-window-dedicated-p window nil)
        (unwind-protect
            (switch-to-buffer (nth (if index (mod (+ index -n) size) 0)
                                   buffers))
          (set-window-dedicated-p window t))))))

(defun ymacs-term|shell-exit (-proc _msg)
  (when (and (eq ymacs-term-exit-action 'shell)
             (not (ymacs-term//switch-internal 0 t)))
    (when-let (window (get-buffer-window (current-buffer)))
      (when (eq window (ymacs-term//get-window))
        (delete-window window))))

  (let ((buffer (process-buffer -proc)))
    (when (and (memq (process-status -proc) '(signal exit))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (if (not (eq ymacs-term-exit-action 'keep))
            (progn
              (when (one-window-p)
                (let ((window (get-buffer-window)))
                  (set-window-dedicated-p window nil)))
              (kill-buffer))

          (let ((buffer-read-only nil))
            (insert (propertize "Press `Ctrl-D' or `q' to kill this buffer. "
                                'font-lock-face
                                'font-lock-comment-face)))

          (setq buffer-read-only t)

          (when-let (map (current-local-map))
            (use-local-map (copy-keymap (current-local-map))))

          (local-set-key (kbd "C-d") #'kill-buffer-and-window)
          (local-set-key (kbd "q") #'kill-buffer-and-window))))))

(defun ymacs-term//wrap-sentinel (&optional -sentinel)
  (lambda (-proc -msg)
    (when -sentinel
      (funcall -sentinel -proc -msg))

    (with-demoted-errors "%s"
      (run-hook-with-args 'ymacs-term-or-comint-process-exit-hook -proc -msg))))

(defsubst ymacs-term//setup-sentinel ()
  (when-let (proc (get-buffer-process (current-buffer)))
    (set-process-sentinel
     proc
     (ymacs-term//wrap-sentinel (process-sentinel proc)))))

(defun ymacs-term//create-vterm-buffer (-buffer -shell-name)
  (let ((vterm-shell -shell-name))
    (with-current-buffer -buffer
      (vterm-mode)
      (ymacs-term//setup-sentinel))))

(defun ymacs-term//create-term-buffer (-buffer -shell-name -term-name)
  ;; Make term, details to see function `make-term' in `term.el'.
  (setq -term-name (substring -term-name 1 (1- (length -term-name))))
  (apply #'make-term -term-name -shell-name
         nil
         ymacs-term-program-arguments)

  (with-current-buffer -buffer
    (term-mode)
    (term-char-mode)
    (ymacs-term//setup-keybindings)
    (ymacs-term//setup-sentinel)
    (setq term-scroll-show-maximum-output nil
          term-scroll-to-bottom-on-output t)))

(defun ymacs-term//create-shell-buffer (-buffer -shell-name -term-name -shell-buffer-p)
  ;; (ymacs-term//setup-sentinel) is called in comint-exec-hook
  (if -shell-buffer-p
      (let ((shell-file-name -shell-name))
        (advice-add
         #'pop-to-buffer
         :override
         (lambda (buffer &rest _)
           (advice-remove #'pop-to-buffer 'temporary)
           (ymacs-term//display-buffer buffer))
         '((name . temporary)))

        (with-current-buffer (shell -buffer)
          (unless (eq major-mode 'shell-mode)
            (shell-mode))))

    (unless (comint-check-proc -buffer)
      (apply #'make-comint-in-buffer -term-name -buffer -shell-name
             nil
             ymacs-term-program-arguments))))

(defun ymacs-term//get-shell-name ()
  (or (unless (eq ymacs-term-type 'shell)
        ymacs-zsh-path)
      ymacs-bash-path))

(defun ymacs-term//create-buffer (&optional -program -shell-buffer-p -full-name)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (let* ((shell-name (or -program
                         (ymacs-term//get-shell-name)
                         (getenv "SHELL")
                         (getenv "ESHELL")
                         "/bin/sh"))
         (term-name (or -full-name
                        (ymacs-term//get-buffer-name
                         (concat "*" ymacs-term-buffer-name "<%s>*"))))
         (default-directory (or default-directory
                                (expand-file-name ymacs-term-initial-directory)))
         (buffer (get-buffer-create term-name)))

    (cond
     ((and (eq ymacs-term-type 'vterm)
           (require 'vterm nil t))
      (ymacs-term//create-vterm-buffer buffer shell-name))
     ((eq ymacs-term-type 'term)
      (ymacs-term//create-term-buffer buffer shell-name term-name))
     (t
      (ymacs-term//create-shell-buffer buffer shell-name term-name -shell-buffer-p)))

    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ymacs-term-exit-action (if -shell-buffer-p 'shell 'keep)))
      buffer)))

(defun ymacs-term//after-prompt-p ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (mark (and proc (process-mark proc)))
         (pos (point)))
    (and mark (>= pos mark))))

(defsubst ymacs-term//extra-env ()
  (run-hook-with-args-until-success 'ymacs-term-environment-functions))

(defun ymacs-term//exec-program (-program -args &optional -name -full-name)
  (let ((ymacs-term-program-arguments -args)
        (ymacs-term-buffer-name -name)
        (ymacs-term-type (if (eq ymacs-term-type 'vterm)
                             'shell
                           ymacs-term-type)))
    (ymacs-term//create-buffer -program nil -full-name)))

(cl-defun ymacs-term//exec-program-reuse-buffer
    (-buffer-name -program -args &key ((:before -before-exec-fn) nil) ((:callback -callback) nil))
  (let* ((buffer-name (concat "*" -buffer-name "*"))
         (buffer (get-buffer-create buffer-name))
         (proc (get-buffer-process buffer)))
    (unless (and proc
                 (process-live-p proc)
                 (eq (buffer-local-value 'major-mode buffer)
                     'term-mode))
      (kill-buffer buffer)
      (when -before-exec-fn
        (funcall -before-exec-fn))
      (setq buffer (ymacs-term//exec-program -program -args nil buffer-name)))

    (when (buffer-live-p buffer)
      (ymacs-term//display-buffer buffer)

      (when -callback
        (with-current-buffer buffer
          (funcall -callback))))))

(defun ymacs-term//create-shell (-directory &optional -new)
  "If there is a term buffer whose default-directory is -DIRECTORY,
return that buffer. Otherwise create a new term buffer.

If -FORCE is non-nil create a new term buffer directly."
  (or (and (not -new)
           (car (--filter
                 (with-current-buffer it
                   (and (ymacs-term//term-buffer-p it)
                        (directory-equal-p -directory default-directory)))
                 (buffer-list))))
      (let ((default-directory -directory))
        (with-temp-env! (ymacs-term//extra-env)
          (ymacs-term//create-buffer nil t)))))

(defun ymacs-term//pop-shell-get-buffer (&optional -arg)
  (when (ymacs-term//term-buffer-p (current-buffer))
    (user-error "Current buffer is already a shell buffer"))

  (let ((new (or (= -arg 0) (>= -arg 16))))
    (ymacs-term//create-shell
     (or (and
          (= -arg 4)
          (run-hook-with-args-until-success 'ymacs-term-directory-functions))
         (and
          new
          (read-directory-name "Directory: " nil nil :mustmatch))
         default-directory)
     new)))

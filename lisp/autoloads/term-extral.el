;;; -*- lexical-binding: t; -*-

;; Kill the buffer when terminal is exited
;;;###autoload
(defvar term-default-directory-function-list '(projectile-project-root))
;;;###autoload
(defvar term-default-environment-function-list nil)

(defvar term--buffer-list nil)
(defvar-local term--ssh-info nil)
(defvar-local term--parent-buffer nil)

;;;###autoload
(defvar-local term-directly-kill-buffer nil)
;;;###autoload
(defvar term-or-comint-process-exit-hook nil)

(defvar term-program-switches nil)
(defvar term-default-directory "~/")
(defvar term-shell-name (or term-zsh-path term-bash-path)
  "The program of term.
If this is nil, setup to environment variable of `SHELL'.")
(defvar term-buffer-name "terminal"
  "The buffer name of term buffer.")

(defvar term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "C-u")
  "The key list that will need to be unbind.")

(defvar term-bind-key-alist
  `(("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term/send-esc)
    ("C-c C-p" . term/previous-line)
    ;; ("C-n" . next-line)
    ("C-/" . term/send-undo)
    ("M-c" . term/send-M-c)
    ("M-u" . term/send-M-u)
    ("M-l" . term/send-M0l)
    ("C-r" . isearch-backward)
    ("C-m" . term/send-return)
    ("C-y" . term-paste)
    ("C-k" . term/kill-line)
    ("M-f" . term/send-forward-word)
    ("M-b" . term/send-backward-word)
    ("M-o" . term/send-backspace)
    ("M-p" . term/send-up)
    ("M-n" . term/send-down)
    ("M-M" . term/send-forward-kill-word)
    ("M-N" . term/send-backward-kill-word)
    ("<C-backspace>" . term/send-backward-kill-word)
    ("<M-backspace>" . term/send-backward-kill-word)
    ("C-DEL" . term/send-backward-kill-word)
    ("M-DEL" . term/send-backward-kill-word)
    ("M-r" . term/send-reverse-search-history)
    ("M-d" . term/send-delete-word)
    ("M-," . term-send-raw)
    ("C-s" . swiper/dispatch)
    ,@(if (display-graphic-p)
          '(("M-]" . term/switch-next)
            ("M-[" . term/switch-prev)
            ("M-}" . term/switch-next)
            ("M-{" . term/switch-prev))
        '(("M-}" . term/switch-next)
          ("M-{" . term/switch-prev)))
    ("C-S-t" . term/pop-shell-current-directory)
    ("C-g" . keyboard-quit))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format.")



(defmacro term//define-send-key (name key doc)
  (declare (indent 2))
  `(defun ,(intern (format "term/send-%s" name)) ()
     ,doc
     (interactive)
     (term-send-raw-string ,key)))

(term//define-send-key esc "\e" "Send ESC in term mode.")
(term//define-send-key return "\C-m" "Send Return in term mode.")
(term//define-send-key backward-kill-word "\C-w" "Backward kill word in term mode.")
(term//define-send-key forward-kill-word "\ed" "Forward kill word in term mode.")
(term//define-send-key reverse-search-history "\C-r" "Search history reverse.")
(term//define-send-key delete-word "\ed" "Delete word in term mode")
(term//define-send-key M-x "\ex"   "Type M-x in term-mode.")
(term//define-send-key up "\ep"   "Type M-p in term-mode.")
(term//define-send-key down "\en"   "Type M-n in term-mode.")
(term//define-send-key M-l "\el"   "Type M-l in term-mode.")
(term//define-send-key M-c "\ec"   "Type M-c in term-mode.")
(term//define-send-key M-u "\eu"   "Type M-u in term-mode.")
(term//define-send-key undo "\C-_"   "Type undo in term-mode.")



(defsubst term//get-buffer-name ($fmt)
  (let* ((index 1)
         (name (format $fmt index)))
    (while (buffer-live-p (get-buffer name))
      (setq name (format $fmt index))
      (setq index (1+ index)))
    name))

;;;###autoload
(defsubst term//wrap-sentinel (&optional sentinel)
  (lambda ($proc $msg)
    (and sentinel (funcall sentinel $proc $msg))
    (ignore-errors (run-hooks 'term-or-comint-process-exit-hook))
    (when (memq (process-status $proc) '(signal exit))
      (with-current-buffer (process-buffer $proc)
        (if term-directly-kill-buffer
            (kill-buffer)
          (let ((buffer-read-only nil))
            (insert (propertize "Press `Ctrl-D' or `q' to kill this buffer. "
                                'font-lock-face 'font-lock-comment-face)))
          (setq buffer-read-only t)
          (when-let (map (current-local-map))
            (use-local-map (copy-keymap (current-local-map))))
          (local-set-key (kbd "C-d") (lambda! (kill-buffer)))
          (local-set-key (kbd "q") (lambda! (kill-buffer))))))))

(defun term//setup-keybindings ()
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defun term//switch-internal ($n)
  (when term--buffer-list
    (let ((size (length term--buffer-list))
          (index (cl-position (current-buffer) term--buffer-list))
          (window (selected-window)))
      (set-window-dedicated-p window nil)
      (unwind-protect
          (switch-to-buffer (nth (if index
                                     (mod (+ index $n) size)
                                   0)
                                 term--buffer-list))
        (set-window-dedicated-p window t)))))

(defun term/switch-next ($create-new)
  (interactive "P")
  (if $create-new
      (term/pop-shell-current-directory)
    (term//switch-internal 1)))

(defun term/switch-prev ($create-new)
  (interactive "P")
  (if $create-new
      (term/pop-shell-current-directory)
    (term//switch-internal -1)))

(defun term//terminal-exit-hook ()
  (setq term--buffer-list
        (delq (current-buffer)
              (--filter (buffer-live-p it) term--buffer-list)))
  (if term--buffer-list
      (term//switch-internal 0)
    (-when-let (window (get-buffer-window (current-buffer)))
      (when (and (window-live-p window)
                 (equal window (term//get-popup-window)))
        (quit-window nil window)))))

(defun term//create-buffer (&optional $shell-program $shell-buffer-p)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (let ((shell-name (or $shell-program
                        term-shell-name
                        (getenv "SHELL")
                        (getenv "ESHELL")
                        "/bin/sh"))
        (term-name (term//get-buffer-name (concat "*" term-buffer-name "<%s>*")))
        (default-directory (or default-directory
                               (expand-file-name term-default-directory))))
    ;; Make term, details to see function `make-term' in `term.el'.
    (setq term-name (substring term-name 1 (1- (length term-name))))
    (-when-let (buffer
                (if term-program-switches
                    (apply #'make-term term-name shell-name nil
                           term-program-switches)
                  (make-term term-name shell-name)))
      (setq term--buffer-list (nconc term--buffer-list (list buffer)))
      (with-current-buffer buffer
        (term-mode)
        (term-char-mode)
        (term//setup-keybindings)
        (setq term-scroll-show-maximum-output nil
              term-scroll-to-bottom-on-output t)
        (-when-let (proc (ignore-errors (get-buffer-process buffer)))
          (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))
        (when $shell-buffer-p
          (add-hook 'term-or-comint-process-exit-hook
                    'term//terminal-exit-hook
                    nil :local)))
      buffer)))


(defun term//after-prompt? ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (mark (and proc (process-mark proc)))
         (pos (point)))
    (and mark (>= pos mark))))

;;;###autoload
(defun term//eval-function-list (variable)
  (let ((functions (symbol-value variable))
        value)
    (while (and functions
                (not (setq value (ignore-errors
                                   (funcall (pop functions)))))))
    value))

;;;###autoload
(defsubst term//extra-env ()
  (term//eval-function-list 'term-default-environment-function-list))

;; (defun term//default-sentinel (proc msg)
;;   (term-sentinel proc msg)
;;   (when (memq (process-status proc) '(signal exit))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((buffer-read-only nil))
;;         (insert (propertize "Press `Ctrl-D' or `q' to kill this buffer. "
;;                             'font-lock-face 'font-lock-comment-face)))
;;       (setq buffer-read-only t)
;;       (local-set-key (kbd "C-d") (lambda! (kill-buffer)))
;;       (local-set-key (kbd "q") (lambda! (kill-buffer))))))

(defun term//get-ssh-info ($arg)
  (let* ((user (file-remote-p default-directory 'user))
         (host (file-remote-p default-directory 'host))
         (method (file-remote-p default-directory 'method))
         port)
    (when (string-match "\\([^#]+\\)#\\([0-9]+\\)" host)
      (setq port (match-string 2 host))
      (setq host (match-string 1 host)))
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
(defun term//exec-program ($program $args &optional $name)
  (let ((term-program-switches $args)
        (term-buffer-name $name))
    (term//create-buffer $program)))

(defun term/switch-back ()
  (interactive)
  (if (and term--parent-buffer (buffer-live-p term--parent-buffer))
      (progn
        (when (eq (term//get-popup-window) (selected-window))
          (pop-to-buffer term--parent-buffer)
          (delete-window (term//get-popup-window))))
    (message "No parent buffer or it was killed !!!")))

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
        (let ((buffer (term//exec-program "ssh" args)))
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
      (let ((default-directory $directory))
        (with-temp-env! (term//extra-env)
          (term//create-buffer nil t)))))

(defsubst term//get-popup-window ()
  (frame-parameter nil 'term-popup-window))

(defsubst term//set-popup-window (popup-window)
  (set-window-dedicated-p popup-window t)
  (set-frame-parameter nil 'term-popup-window popup-window))

;;;###autoload
(defun term//pop-to-buffer (buffer)
  (let ((popup-window (term//get-popup-window)))
    (if (and (window-live-p popup-window)
             (eq 'term-mode
                 (buffer-local-value 'major-mode (window-buffer popup-window))))
        ;; Reuse window
        (progn
          (select-window popup-window)
          (set-window-dedicated-p popup-window nil)
          (set-window-buffer popup-window buffer))
      (pop-to-buffer buffer)
      (term//set-popup-window (get-buffer-window buffer)))))

;;;###autoload
(defun term/pop-shell-current-directory ()
  (interactive)
  (when-let ((parent-buffer (or term--parent-buffer (current-buffer)))
             (buffer (term//create-buffer nil t)))
    (with-current-buffer buffer
      (setq term-directly-kill-buffer t)
      (local-set-key [f8] #'term/switch-back)
      (local-set-key (kbd "C-c C-z") #'term/switch-back-no-quit)
      (setq term--parent-buffer parent-buffer))
    (term//pop-to-buffer buffer)))

(defun term//pop-shell-get-buffer (&optional $arg)
  (unless (memq major-mode '(eshell-mode term-mode shell-mode))
    (if (file-remote-p default-directory)
        (apply #'term/ssh (term//get-ssh-info $arg))
      (term//local-shell (or (and (= $arg 4)
                                  (term//eval-function-list
                                   'term-default-directory-function-list))
                             default-directory)
                         (= $arg 16)))))

;;;###autoload
(defun term/pop-shell (&optional $arg)
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term."
  (interactive "p")
  (when-let* ((buffer (term//pop-shell-get-buffer $arg))
              (parent-buffer (current-buffer)))
    (with-current-buffer buffer
      (setq term-directly-kill-buffer t))
    (if (= $arg 0)
        (switch-to-buffer buffer)
      (with-current-buffer buffer
        (local-set-key [f8] #'term/switch-back)
        (local-set-key (kbd "C-c C-z") #'term/switch-back-no-quit)
        (setq term--parent-buffer parent-buffer))
      (term//pop-to-buffer buffer))))

;;;###autoload
(defun term/conditional-send-raw ()
  (interactive)
  (let ((command (global-key-binding (this-command-keys))))
    ;; When `point' is after last output mark, send raw string
    ;; Otherwise call global binding
    (call-interactively (if (and command (term//after-prompt?))
                            #'term-send-raw
                          command))))

(defun term/kill-line ()
  (interactive)
  (if (term//after-prompt?)
      (progn
        (kill-new (buffer-substring (point) (line-end-position)))
        (term-send-raw-string "\C-k"))
    (call-interactively #'kill-line)))

(defun term/send-backward-word ()
  (interactive)
  (if (term//after-prompt?)
      (term-send-raw-string "\eb")
    (call-interactively #'backward-word)))

(defun term/send-forward-word ()
  (interactive)
  (if (term//after-prompt?)
      (term-send-raw-string "\ef")
    (call-interactively #'forward-word)))

(defun term/previous-line ()
  (interactive)
  ;; Fix for Emacs 26
  (setq term-goto-process-mark nil)
  (forward-line -1))

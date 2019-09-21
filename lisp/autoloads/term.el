;;; -*- lexical-binding: t; -*-

(require 'tramp)
(eval-when-compile
  (require 'term))

;; Kill the buffer when terminal is exited
;;;###autoload
(defvar term-default-directory-function-list '(projectile-project-root))
;;;###autoload
(defvar term-default-environment-function-list nil)

(defvar-local term--ssh-info nil)
(defvar-local term--parent-buffer nil)
(defvar-local term--extra-name nil)

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

(defconst term--setup-directory-tracking
  '((term-mode
     ("zsh" . " chpwd() { printf '\\033AnSiTu %s\\n' \"$USER\"; print -P '\\033AnSiTc %d'; }; clear\n"))
    (vterm-mode
     ("zsh" . " chpwd() { print -Pn '\\e]51;A$(pwd)\\e\\\\'; }; clear\n")
     ("bash" . " cd() { builtin cd \"$@\" || return; [ \"$OLDPWD\" = \"$PWD\" ] || echo -e \"\\e]51;A$(pwd)\\e\\\\\"; }; clear\n"))))

(defvar term-bind-key-alist
  `(("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term/send-esc)
    ("C-c C-l" . term-line-mode)
    ;; ("C-n" . next-line)
    ("C-/" . term/send-undo)
    ("M-c" . term/send-M-c)
    ("M-u" . term/send-M-u)
    ("M-l" . term/send-M-l)
    ("C-m" . term/send-return)
    ("C-y" . term-paste)
    ("C-k" . term/kill-line)
    ("M-f" . term/send-forward-word)
    ("M-b" . term/send-backward-word)
    ("M-p" . term/send-up)
    ("M-n" . term/send-down)
    ("<C-backspace>" . term/send-backward-kill-word)
    ("<M-backspace>" . term/send-backward-kill-word)
    ("C-DEL" . term/send-backward-kill-word)
    ("M-DEL" . term/send-backward-kill-word)
    ("M-r" . term/send-reverse-search-history)
    ("M-d" . term/send-delete-word)
    ("M-," . term-send-raw)
    ("M-y" . term/yank-pop)
    ("C-s" . term/swiper)
    ("M-}" . term/switch-next)
    ("M-{" . term/switch-prev)
    ("M-o" . term/ivy-switch)
    ("M-N" . term/set-extra-name)
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
(term//define-send-key M-x "\ex" "Type M-x in term-mode.")
(term//define-send-key up "\ep" "Type M-p in term-mode.")
(term//define-send-key down "\en" "Type M-n in term-mode.")
(term//define-send-key M-l "\el" "Type M-l in term-mode.")
(term//define-send-key M-c "\ec" "Type M-c in term-mode.")
(term//define-send-key M-u "\eu" "Type M-u in term-mode.")
(term//define-send-key undo "\C-_" "Type undo in term-mode.")



(defsubst term//get-popup-window ()
  (when-let (window (frame-parameter nil 'term-popup-window))
    (and (window-live-p window) window)))

(defsubst term//set-popup-window (popup-window)
  (set-window-dedicated-p popup-window t)
  (set-frame-parameter nil 'term-popup-window popup-window))

(defsubst term//get-buffer-name (-fmt)
  (let* ((index 1)
         (name (format -fmt index)))
    (while (buffer-live-p (get-buffer name))
      (setq name (format -fmt index))
      (setq index (1+ index)))
    name))

;;;###autoload
(defun term//wrap-sentinel (&optional sentinel)
  (lambda (-proc -msg)
    (and sentinel (funcall sentinel -proc -msg))
    (ignore-errors (run-hooks 'term-or-comint-process-exit-hook))
    (when (memq (process-status -proc) '(signal exit))
      (with-current-buffer (process-buffer -proc)
        (if term-directly-kill-buffer
            (progn
              (when (one-window-p)
                (let ((window (get-buffer-window)))
                  (set-window-dedicated-p window nil)))
              (kill-buffer))
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

(defsubst term//term-buffer-p (buffer)
  (and (buffer-live-p buffer)
       (not (string-prefix-p " " (buffer-name buffer)))
       (with-current-buffer buffer
         (derived-mode-p 'term-mode 'shell-mode 'eshell-mode 'vterm-mode))))

(defsubst term//window-display-term-buffer-p (window)
  (and (window-live-p window)
       (term//term-buffer-p (window-buffer window))))

(defun term//get-buffer-list ()
  (--sort
   (string< (buffer-name it) (buffer-name other))
   (--filter (term//term-buffer-p it) (buffer-list))))

(defun term//switch-internal (-n &optional -ignore-self)
  (let ((term-buffer-list (term//get-buffer-list)))
    (when -ignore-self
      (setq term-buffer-list (remove (current-buffer) term-buffer-list)))
    (when term-buffer-list
      (let ((size (length term-buffer-list))
            (index (cl-position (current-buffer) term-buffer-list))
            (window (selected-window)))
        (set-window-dedicated-p window nil)
        (unwind-protect
            (switch-to-buffer (nth (if index (mod (+ index -n) size) 0)
                                   term-buffer-list))
          (set-window-dedicated-p window t))))))


(defun term/ivy-switch ()
  (interactive)
  (unless (term//term-buffer-p (current-buffer))
    (user-error "You can switch to another term buffer from a term buffer"))
  (let ((term-buffer-list
         (mapcar (lambda (buffer)
                   (let ((name (buffer-local-value 'term--extra-name buffer)))
                     (cons (if name
                               (concat (buffer-name buffer) ": " name)
                             (buffer-name buffer))
                           buffer)))
                 (term//get-buffer-list))))
    (if (<= (length term-buffer-list) 1)
        (user-error "There is only one term buffer")
      (ivy-read "Switch to term buffer: " term-buffer-list
                :require-match t
                :action (lambda (candidate)
                          (let ((window (selected-window)))
                            (set-window-dedicated-p window nil)
                            (unwind-protect
                                (switch-to-buffer (cdr candidate))
                              (set-window-dedicated-p window t))))))))

(defun term/switch-next (-create-new)
  (interactive "P")
  (if -create-new
      (term/pop-shell-current-directory)
    (term//switch-internal 1)))

(defun term/switch-prev (-create-new)
  (interactive "P")
  (if -create-new
      (term/pop-shell-current-directory)
    (term//switch-internal -1)))

(defun term//terminal-exit-hook ()
  (unless (term//switch-internal 0 t)
    (when-let (window (get-buffer-window (current-buffer)))
      (when (eq window (term//get-popup-window))
        (delete-window window)))))

(defun term//create-buffer (&optional -program -shell-buffer-p)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (let ((shell-name (or -program
                        term-shell-name
                        (getenv "SHELL")
                        (getenv "ESHELL")
                        "/bin/sh"))
        (term-name (term//get-buffer-name (concat "*" term-buffer-name "<%s>*")))
        (default-directory (or default-directory
                               (expand-file-name term-default-directory)))
        buffer)
    (if (and term-prefer-vterm (require 'vterm nil t))
        (let ((vterm-shell shell-name))
          (with-current-buffer (setq buffer (get-buffer-create term-name))
            (vterm-mode)))
      ;; Make term, details to see function `make-term' in `term.el'.
      (setq term-name (substring term-name 1 (1- (length term-name))))
      (setq buffer (if term-program-switches
                       (apply #'make-term term-name shell-name nil term-program-switches)
                     (make-term term-name shell-name)))
      (when buffer
        (with-current-buffer buffer
          (term-mode)
          (term-char-mode)
          (term//setup-keybindings)
          (setq term-scroll-show-maximum-output nil
                term-scroll-to-bottom-on-output t))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let (proc (ignore-errors (get-buffer-process buffer)))
          (set-process-sentinel proc (term//wrap-sentinel (process-sentinel proc))))
        (when -shell-buffer-p
          (add-hook 'term-or-comint-process-exit-hook 'term//terminal-exit-hook nil :local)
          (when-let (setup-code (-some->> term--setup-directory-tracking
                                          (assoc major-mode)
                                          (assoc (file-name-base shell-name))
                                          cdr-safe))
            (if (eq major-mode 'term-mode)
                (term-send-raw-string setup-code)
              (vterm-send-string setup-code)))))
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

;;;###autoload
(defun term//exec-program (-program -args &optional -name)
  (let ((term-program-switches -args)
        (term-buffer-name -name)
        (term-prefer-vterm nil))
    (term//create-buffer -program)))

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

(defsubst term//parse-sconfig ()
  (remove nil (mapcar #'cadr (tramp-parse-sconfig "~/.ssh/config"))))

(defun term//get-ssh-info ()
  (let* ((address (completing-read "[user[#port]@]host: " (term//parse-sconfig)
                                   nil  ; predicate
                                   nil  ; require-match
                                   (let ((user (file-remote-p default-directory 'user))
                                         (host (file-remote-p default-directory 'host)))
                                     (if user (concat user "@" host) host)))))
    (unless (string-match "\\(?:\\([^#]+\\)#\\([0-9]+\\)@\\)?\\(.+\\)"
                          address)
      (user-error "Invalid address."))
    (let ((data (cddr (match-data)))
          result)
      (while data
        (let ((beg (pop data))
              (end (pop data)))
          (push (when beg (substring address beg end)) result)))
      result)))

(defun term//ssh-initialize (-host)
  (term-send-raw-string (format "
chpwd() {
    printf '\\033AnSiTu %%s\n' \"sshx:$USER\"
    print -P \"\\033AnSiTc %%d\"
    printf '\\033AnSiTh %s\n'
}
chpwd;clear
" -host)))

;;;###autoload
(defun term//create-ssh-buffer (-host -port -user &optional -force)
  (interactive)
  (let ((args (cl-list* (if -user (format "%s@%s" -user -host) -host)
                        (unless (or (not -port)
                                    (zerop (string-to-number -port)))
                          (list "-p" (format "%d" -port))))))
    (or (and (not -force)
             (car (--filter (with-current-buffer it
                              (and (eq major-mode 'term-mode)
                                   (equal args term--ssh-info)
                                   (process-live-p (get-buffer-process it))))
                            (buffer-list))))
        (let ((buffer (term//exec-program "ssh" args "ssh")))
          (with-current-buffer buffer
            (term//ssh-initialize -host)
            (setq term--ssh-info args))
          buffer))))

(defun term//create-local-shell (-directory &optional -force)
  "If there is a term buffer whose default-directory is -DIRECTORY,
return that buffer. Otherwise create a new term buffer.

If -FORCE is non-nil create a new term buffer directly."
  (or (and (not -force)
           (car (--filter (with-current-buffer it
                            (and (term//term-buffer-p it)
                                 (directory-equal-p -directory default-directory)))
                          (buffer-list))))
      (let ((default-directory -directory))
        (with-temp-env! (term//extra-env)
          (term//create-buffer nil t)))))

(defun term//pop-shell-get-buffer (&optional -arg)
  (unless (term//term-buffer-p (current-buffer))
    (let ((force (or (= -arg 0) (>= -arg 16))))
      (if (file-remote-p default-directory)
          (apply #'term//create-ssh-buffer (append (term//get-ssh-info) (list force)))
        (term//create-local-shell
         (or (and (= -arg 4) (term//eval-function-list 'term-default-directory-function-list))
             (and force (read-directory-name "Directory: " nil nil :mustmatch))
             default-directory)
         force)))))

;;;###autoload
(defun term//pop-to-buffer (buffer)
  (let ((popup-window
         (car-safe (cl-remove-if-not #'term//window-display-term-buffer-p
                                     (cl-list* (term//get-popup-window) (window-list))))))
    (if popup-window
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

;;;###autoload
(defun term/ssh ()
  (interactive)
  (let ((buffer (apply #'term//create-ssh-buffer (term//get-ssh-info))))
    (with-current-buffer buffer
      (setq term-directly-kill-buffer nil))
    (term//pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun term/pop-shell (&optional -arg)
  "Popup to a term buffer.
-ARG = 0: create a term buffer in current window
-ARG >= 16: try to find a old term buffer by given directory
else: try to find a old term buffer and pop to it"
  (interactive "p")
  (when-let* ((buffer (term//pop-shell-get-buffer -arg))
              (parent-buffer (current-buffer)))
    (with-current-buffer buffer
      (setq term-directly-kill-buffer t))
    (if (= -arg 0)
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

(with-eval-after-load 'counsel
  (add-to-list 'ivy-format-functions-alist '(term/yank-pop . counsel--yank-pop-format-function)))

(defun term/yank-pop (&optional _)
  (interactive "P")
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank)
               (if (eq major-mode 'vterm-mode)
                   (lambda (str) (vterm-send-string str t))
                 (lambda (str) (term-send-raw-string str)))))
      (call-interactively #'counsel-yank-pop))))

(defun term/kill-line ()
  (interactive)
  (if (eq major-mode 'term-mode)
      (if (term//after-prompt?)
          (progn
            (kill-new (buffer-substring (point) (line-end-position)))
            (term-send-raw-string "\C-k"))
        (call-interactively #'kill-line))
    (kill-new (buffer-substring (point) (line-end-position)))
    (call-interactively #'vterm--self-insert)))

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

(defun term/swiper ()
  (interactive)
  (unless (term//term-buffer-p (current-buffer))
    (user-error "Not in `term-mode' buffer"))
  (if (eq major-mode 'term-mode)
      (term-line-mode)
    (vterm-copy-mode 1))

  (call-interactively 'swiper/dispatch))

;;;###autoload
(defun term/shell-command-on-server (&optional -arg)
  (interactive "P")
  (when (eq major-mode 'term-mode)
    (user-error "Current buffer is term buffer"))
  (let* ((filename (abbreviate-file-name (buffer-file-name)))
         (command (read-shell-command
                   "Run: "
                   (or (and (boundp 'executable-command) executable-command)
                       (and (boundp 'remote-shell-command-history)
                            (car-safe remote-shell-command-history))
                       filename)
                   'remote-shell-command-history))
         (buffer (term/ssh)))
    (with-current-buffer buffer
      (term-send-raw-string
       (format " cd %s;git pull;%s\n"
               (abbreviate-file-name
                (or (and -arg (read-directory-name "Run in: " nil nil t))
                    (projectile-ensure-project (projectile-project-root))))
               command)))))

;;;###autoload
(defun term/set-extra-name ()
  (interactive)
  (unless (term//term-buffer-p (current-buffer))
    (user-error "Not in a term buffer"))
  (let ((name (read-from-minibuffer "Extra name: " term--extra-name nil nil 'term-extra-name-history)))
    (setq term--extra-name (if (string= name "") nil name))))

;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'term))

(defsubst ymacs-term//get-buffer-name (-fmt)
  (let* ((index 1)
         (name (format -fmt index)))
    (while (buffer-live-p (get-buffer name))
      (setq name (format -fmt index))
      (setq index (1+ index)))
    name))

(defun ymacs-term//keybindings-setup ()
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (dolist (unbind-key ymacs-term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; @see `ymacs-term-bind-key-alist'
    (dolist (element ymacs-term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defsubst ymacs-term//shell-buffer-p (-buffer)
  (and (buffer-live-p -buffer)
       (with-current-buffer -buffer
         (derived-mode-p 'term-mode 'shell-mode 'eshell-mode 'vterm-mode))))

(defun ymacs-term//switch-internal (-n)
  (let ((buffers (cl-remove-if-not
                  (lambda (buffer)
                    (or (eq buffer (current-buffer))
                        (process-live-p (get-buffer-process buffer))))
                  (ymacs-popup//get-term-buffer-list))))
    (when buffers
      (let* ((size (length buffers))
             (index (cl-position (current-buffer) buffers))
             ;; 1 means to switch to last term buffer
             (index (if index (mod (+ size (- index -n)) size) 0))
             (window (selected-window)))
        (unless (= size 1)
          (set-window-dedicated-p window nil)
          (switch-to-buffer (nth index buffers) 'norecord)
          (ymacs-popup//set-term-window window)
          window)))))

(defun ymacs-term//shell-exit ()
  (let ((buffer (current-buffer)))
    (let ((window (get-buffer-window)))
      (when (and (window-live-p window)
                 (eq window (ymacs-popup//get-term-window))
                 (eq ymacs-term-exit-action 'shell)
                 ;; try to switch to next shell  buffer
                 (not (ymacs-term//switch-internal 1)))
        (if (one-window-p)
            (quit-window window)
          (delete-window window))))

    (when (buffer-live-p buffer)
      ;; the buffer maybe killed or buried
      (with-current-buffer buffer
        (if (not (eq ymacs-term-exit-action 'keep))
            (progn
              (when (one-window-p)
                (set-window-dedicated-p (get-buffer-window) nil))
              (kill-buffer))

          (let ((help-str (propertize
                           "[Press `Ctrl-D' or `q' to kill this buffer. ]"
                           'font-lock-face
                           'font-lock-warning-face)))
            (if (listp mode-line-buffer-identification)
                (add-to-list 'mode-line-buffer-identification help-str t)
              (setq mode-line-buffer-identification
                    (list mode-line-buffer-identification help-str))))

          (when-let (map (current-local-map))
            (use-local-map (copy-keymap (current-local-map))))

          (local-set-key (kbd "C-d") #'kill-buffer-and-window)
          (local-set-key (kbd "q") #'kill-buffer-and-window))))))

(defsubst ymacs-term//sentinel-setup ()
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel
     proc
     (make-process-sentinel!
      nil nil
      (lambda ()
        (run-hook-with-args 'ymacs-term-process-exit-hook))
      (process-sentinel proc)))))

(defsubst ymacs-term//get-shell-name ()
  (or ymacs-zsh-path
      ymacs-bash-path))

(defun ymacs-term//create-vterm-buffer (-buffer -shell-name)
  (let ((vterm-shell -shell-name))
    (with-current-buffer -buffer
      (vterm-mode)
      (ymacs-term//sentinel-setup))))

(defun ymacs-term//create-term-buffer (-buffer -shell-name -term-name)
  ;; Make term, details to see function `make-term' in `term.el'.
  (setq -term-name (substring -term-name 1 (1- (length -term-name))))
  (apply #'make-term -term-name -shell-name
         nil
         ymacs-term-program-arguments)

  (with-current-buffer -buffer
    (term-mode)
    (term-char-mode)
    (ymacs-term//keybindings-setup)
    (ymacs-term//sentinel-setup)
    (setq term-scroll-show-maximum-output nil)
    (setq term-scroll-to-bottom-on-output t)))

(defun ymacs-term//create-shell-buffer (-buffer -shell-name -term-name -shell-buffer-p)
  ;; (ymacs-term//sentinel-setup) is called in comint-exec-hook
  (if -shell-buffer-p
      (let ((shell-file-name -shell-name))
        (with-current-buffer (shell -buffer)
          (unless (eq major-mode 'shell-mode)
            (shell-mode))))

    (unless (comint-check-proc -buffer)
      (apply #'make-comint-in-buffer -term-name -buffer -shell-name
             nil
             ymacs-term-program-arguments))))

(defun ymacs-term//create-buffer (&optional -program -shell-buffer-p -full-name)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input."
  (let* ((ymacs-term-type (if (and default-directory (file-remote-p default-directory))
                              'shell
                            ymacs-term-type))
         (shell-name (or -program
                         (ymacs-term//get-shell-name)
                         (getenv "SHELL")
                         (getenv "ESHELL")
                         "/bin/sh"))
         (term-name (or -full-name
                        (ymacs-term//get-buffer-name
                         (concat "*" ymacs-term-buffer-name "<%s>*"))))
         (buffer (get-buffer-create term-name)))

    (with-temp-env! (ymacs-editor//get-environment)
      (cond
       ((and (eq ymacs-term-type 'vterm)
             (require 'vterm nil t))
        (ymacs-term//create-vterm-buffer buffer shell-name))
       ((eq ymacs-term-type 'term)
        (ymacs-term//create-term-buffer buffer shell-name term-name))
       ((eq ymacs-term-type 'shell)
        (ymacs-term//create-shell-buffer buffer shell-name term-name -shell-buffer-p))
       (t (user-error "Unsupported shell type %s" ymacs-term-type))))

    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when -shell-buffer-p
          (hack-dir-local-variables-non-file-buffer)

          (run-hooks 'ymacs-term-shell-exec-hook))
        (setq ymacs-term-exit-action (if -shell-buffer-p 'shell 'keep)))
      buffer)))

(defsubst ymacs-term//after-prompt-p ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (mark (and proc (process-mark proc)))
         (pos (point)))
    (and mark (>= pos mark))))

(defun ymacs-term//send-string (-string)
  (cond
   ((eq major-mode 'term-mode)
    (term-send-raw-string -string))
   ((eq major-mode 'vterm-mode)
    (vterm-send-string -string))
   ((derived-mode-p 'comint-mode)
    (comint-send-string (get-buffer-process (current-buffer)) -string))
   (t (user-error "Unsupported shell type %s" major-mode))))

(defun ymacs-term//exec-program (-program -args &optional -name -full-name)
  (let ((ymacs-term-program-arguments -args)
        (ymacs-term-buffer-name -name))
    (ymacs-term//create-buffer -program nil -full-name)))

(cl-defun ymacs-term//exec-program-in-buffer
    (-buffer-name &key -program -program-args (-callback nil))
  (declare (indent 1))
  (let* ((buffer-name (concat "*" -buffer-name "*"))
         (buffer (get-buffer-create buffer-name))
         (proc (get-buffer-process buffer)))
    (unless (and proc (process-live-p proc))
      (kill-buffer buffer)
      (setq buffer (ymacs-term//exec-program -program -program-args nil buffer-name)))

    (when (buffer-live-p buffer)
      (display-buffer buffer)

      (when -callback
        (with-current-buffer buffer
          (funcall -callback))))))

(defsubst ymacs-term//get-directory ()
  (run-hook-with-args-until-success 'ymacs-term-directory-functions))

(defsubst ymacs-term//get-shell-buffer-in-directory (-directory)
  (cl-loop
   for buffer in (buffer-list)
   for directory = (buffer-local-value 'default-directory buffer)
   when (and (ymacs-term//shell-buffer-p buffer)
             (equal-directory! directory -directory))
   return buffer))

(defun ymacs-term//pop-shell-get-buffer (&optional -arg)
  (when (ymacs-term//shell-buffer-p (current-buffer))
    (user-error "Current buffer is already a shell buffer"))

  (let* ((force-new-buffer (or (= -arg 0) (>= -arg 16)))
         (directory
          (or (when (= -arg 4)
                (ymacs-term//get-directory))
              (when force-new-buffer
                (read-directory-name "Shell in: " nil nil :mustmatch))
              default-directory)))

    (or (when (not force-new-buffer)
          (ymacs-term//get-shell-buffer-in-directory directory))

        (let ((default-directory directory))
          (ymacs-term//create-buffer nil t)))))

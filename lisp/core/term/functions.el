;;; -*- lexical-binding: t; -*-

(defsubst ymacs-term//get-type ()
  (or (and default-directory
           (file-remote-p default-directory)
           'shell)
      ymacs-term-type))

(defsubst ymacs-term//get-shell-file-name ()
  (let (host)
    (if (and default-directory
             (setq host (file-remote-p default-directory 'host)))
        (or (cdr-safe (assoc-string host ymacs-term-path-alist))
            (let ((path (file-local-name
                         (expand-file-name
                          (read-file-name "Remote shell path: " nil
                                          shell-file-name t
                                          shell-file-name)))))
              (custom-set-variables
               `(ymacs-term-path-alist '((,host . ,path) . ,ymacs-term-path-alist)))
              (when (yes-or-no-p "Save `ymacs-term-path-alist' to file?")
                (custom-save-all))))
      shell-file-name)))

(defsubst ymacs-term//get-buffer-name (-fmt)
  (let* ((index 1)
         (name (format -fmt index)))
    (while (buffer-live-p (get-buffer name))
      (setq name (format -fmt index))
      (setq index (1+ index)))
    name))

(defsubst ymacs-term//shell-buffer-p (-buffer)
  (and (buffer-live-p -buffer)
       (with-current-buffer -buffer
         (and
          (derived-mode-p 'term-mode 'shell-mode 'eshell-mode 'ghostel-mode)
          (not (bound-and-true-p ymacs-term--is-not-a-shell))))))

(defun ymacs-term//switch-internal (-n)
  (let ((buffers (ymacs-popup//get-term-buffer-list)))
    (when buffers
      (let* ((size (length buffers))
             (index (cl-position (current-buffer) buffers))
             ;; 1 means to switch to last term buffer
             (index (if index (mod (+ size (- index -n)) size) 0))
             (window (selected-window)))
        (unless (= size 1)
          (set-window-dedicated-p window nil)
          (switch-to-buffer (nth index buffers))
          (ymacs-popup//set-term-window window)
          window)))))

(defun ymacs-term//quit-window (-window)
  (interactive (list (selected-window)))
  (when (and (window-live-p -window)
             (eq -window (ymacs-popup//get-term-window))
             ;; try to switch to next shell  buffer
             (not (ymacs-term//switch-internal 1)))
    ;; restore to previous-buffer
    (let ((force-delete (eq (nth 1 (window-parameter -window 'quit-restore)) 'window)))
      (quit-window nil -window)
      (when (and force-delete
                 (window-live-p -window))
        (delete-window -window)))))

(defun ymacs-term//kill-buffer-and-quit-window ()
  (interactive)
  (let ((buffer (current-buffer)))
    (ymacs-term//quit-window (selected-window))
    (kill-buffer buffer)))

(defun ymacs-term//set-quit-keys (&optional -try-next-p)
  (let ((help-str (propertize
                   "[Press `Ctrl-D' or `q' to kill this buffer. ]"
                   'font-lock-face
                   'font-lock-warning-face)))
    (if (listp mode-line-buffer-identification)
        (add-to-list 'mode-line-buffer-identification help-str t)
      (setq mode-line-buffer-identification
            (list mode-line-buffer-identification help-str)))
    (let ((map (copy-keymap (current-local-map))))
      (if -try-next-p
          (progn
            (define-key map (kbd "C-d") #'ymacs-term//kill-buffer-and-quit-window)
            (define-key map (kbd "q") #'ymacs-term//quit-window))
        (define-key map (kbd "C-d") #'kill-buffer-and-window)
        (define-key map (kbd "q") #'quit-window))
      (use-local-map map))))

(defun ymacs-term//shell-exit ()
  (let ((buffer (current-buffer)))
    (when (eq ymacs-term-exit-action 'shell)
      (ymacs-term//quit-window (get-buffer-window)))

    (when (buffer-live-p buffer)
      ;; the buffer maybe killed or buried
      (with-current-buffer buffer
        (if (not (eq ymacs-term-exit-action 'keep))
            (progn
              (when (one-window-p)
                (set-window-dedicated-p (get-buffer-window) nil))
              (kill-buffer))

          (ymacs-term//set-quit-keys t))))))

(defsubst ymacs-term//sentinel-setup ()
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel
     proc
     (make-process-sentinel!
      nil nil
      (lambda ()
        (run-hook-with-args 'ymacs-term-process-exit-hook))
      (process-sentinel proc)))))

(defun ymacs-term//ghostel-exit (-buffer _event)
  "Run `ymacs-term-process-exit-hook' when a ghostel terminal exits.
Added to `ghostel-exit-functions'.  `ghostel-kill-buffer-on-exit' is
kept nil (see config.el) so our hook, not ghostel, decides the buffer's
fate via `ymacs-term-exit-action'.

`ghostel-exit-functions' runs in the middle of ghostel's own sentinel,
which keeps operating on -BUFFER afterwards, so defer to a timer to let
the sentinel finish before we may kill the buffer."
  (when (buffer-live-p -buffer)
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p -buffer)
         (with-current-buffer -buffer
           (run-hook-with-args 'ymacs-term-process-exit-hook)))))))

(defun ymacs-term//create-ghostel-buffer (-term-name -shell-name -program)
  "Create and return a `ghostel-mode' terminal buffer named -TERM-NAME.

With no explicit -PROGRAM this is an interactive shell, so we spawn it
through `ghostel-create': that goes via `ghostel--start-process', which
injects ghostel's bundled shell integration.  The integration's OSC 7
reports are what make `default-directory' track the shell's working
directory (and enable prompt navigation).  `ghostel-exec' deliberately
skips integration -- it is for arbitrary programs -- so a shell started
with it cannot dirtrack.

With an explicit -PROGRAM (a REPL, see `ymacs-term//exec-program') we do
want `ghostel-exec': it runs the argv directly with no integration and,
crucially, no macOS `login' wrap, which `ghostel--start-process' would
otherwise apply to the program.

Exit handling goes through `ghostel-exit-functions' (see config.el), so
we don't install a process sentinel here."
  (let ((ghostel-kill-buffer-on-exit nil))
    (if -program
        (let ((buffer (get-buffer-create -term-name)))
          (ghostel-exec buffer -shell-name ymacs-term-program-arguments)
          buffer)
      (let ((ghostel-shell (if ymacs-term-program-arguments
                               (cons -shell-name ymacs-term-program-arguments)
                             -shell-name)))
        (ghostel-create -term-name)))))

(defun ymacs-term//create-shell-buffer (-buffer -shell-name -term-name -shell-buffer-p)
  ;; (ymacs-term//sentinel-setup) is called in comint-exec-hook
  (if -shell-buffer-p
      (let ((explicit-shell-file-name -shell-name))
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
  (let* ((term-type (ymacs-term//get-type))
         (shell-name (or -program
                         (ymacs-term//get-shell-file-name)))
         (term-name (or -full-name
                        (ymacs-term//get-buffer-name
                         (concat "*" ymacs-term-buffer-name "<%s>*"))))
         buffer)

    (with-temp-env! (ymacs-editor//get-environment)
      (cond
       ((and (eq term-type 'ghostel)
             (require 'ghostel nil t))
        ;; `ghostel-create' generates its own buffer (so shell integration /
        ;; dirtracking is wired up), so let it own creation instead of
        ;; pre-creating one that would collide by name.
        (setq buffer (ymacs-term//create-ghostel-buffer term-name shell-name -program)))
       ((eq term-type 'shell)
        (setq buffer (get-buffer-create term-name))
        (ymacs-term//create-shell-buffer buffer shell-name term-name -shell-buffer-p))
       (t (user-error "Unsupported shell type %s" term-type))))

    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when -shell-buffer-p
          (hack-dir-local-variables-non-file-buffer)

          (run-hooks 'ymacs-term-shell-exec-hook))
        (setq ymacs-term-exit-action (if -shell-buffer-p 'shell 'keep)))
      buffer)))

(defun ymacs-term//send-string (-string)
  (cond
   ((eq major-mode 'ghostel-mode)
    (ghostel-send-string -string))
   ((derived-mode-p 'comint-mode)
    (comint-send-string (get-buffer-process (current-buffer)) -string))
   (t (user-error "Unsupported shell type %s" major-mode))))

(defun ymacs-term//exec-program (-program -args &optional -name -full-name)
  (let ((ymacs-term-program-arguments -args)
        (ymacs-term-buffer-name -name))
    (ymacs-term//create-buffer -program nil -full-name)))

(cl-defun ymacs-term//exec-program-in-buffer
    (-buffer-name &key ((:program -program)) ((:program-args -program-args)) ((:callback -callback)))
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

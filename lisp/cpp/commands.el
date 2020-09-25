;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cmacexp))

(defun ymacs-cpp//compile-command (-directory)
  "Return suitable compile command for current project"
  (cond
   ((bound-and-true-p projectile-project-compilation-cmd)
    projectile-project-compilation-cmd)
   ((file-exists-p (expand-file-name "build.ninja" -directory))
    (concat "ninja -C " -directory))
   ((file-exists-p (expand-file-name "Makefile" -directory))
    (concat "make -j 4 --no-print-directory -C" -directory))
   (t nil)))

;;;###autoload
(defun ymacs-cpp/load-in-repl ()
  (interactive)
  (cond
   ((file-remote-p default-directory)
    (message "Not support in remove sever !"))
   ((not (executable-find "root"))
    (message "Executable `root' not found !"))
   ((not (buffer-file-name))
    (message "Buffer has no file !"))
   (t
    (let ((file (buffer-file-name)))
      (ymacs-term//exec-program-reuse-buffer
       (concat "ROOT:" (buffer-name))
       "root" (list "-l" (or file ""))
       :callback
       (lambda () (term-send-raw-string (format ".X %s\n" file))))))))

;;;###autoload
(defun ymacs-cpp/debug-current-file (&optional -new-session)
  (interactive "P")
  (require 'realgud nil t)

  (let ((cmd-bufs
         (cl-remove-if-not
          (lambda (x)
            (and (realgud-cmdbuf? x)
                 (process-live-p (get-buffer-process x))))
          (buffer-list))))
    (if (or -new-session (not cmd-bufs))
        (let ((default-directory
                (or (and -new-session default-directory)
                    (expand-file-name
                     (read-directory-name
                      "Directory: "
                      (file-name-as-directory
                       (or (ymacs-cpp//build-dir)
                           default-directory))
                      nil
                      :must-match))))
              (gud-chdir-before-run nil))
          (realgud:gdb
           (or (ignore-errors (realgud:gdb-query-cmdline "gdb"))
               (read-shell-command "Run like this: " nil
                                   'realgud:gdb-minibuffer-history))))
      (unless realgud-short-key-mode
        (realgud-short-key-mode 1)))))

;;;###autoload
(defun ymacs-cpp/compile (-no-prompt-p)
  (interactive "P")
  (with-temp-env! (ymacs-cpp//env)
    (let ((build-dir (ymacs-cpp//build-dir)))
      (if-let (command (and -no-prompt-p (ymacs-cpp//compile-command build-dir)))
          (compile command)
        (let ((commands
               (cl-remove-duplicates
                (cl-remove
                 nil
                 `(,(when build-dir
                      (ymacs-cpp//compile-command build-dir))
                   ,(when-let (dir (locate-topmost! "Makefile"))
                      (ymacs-cpp//compile-command dir))
                   ,@(--map
                      (ignore-errors (funcall it))
                      ymacs-cpp-buffer-command-functions)
                   ,@(-take 2 compile-history)
                   ,compile-command)))))
          (let ((max-mini-window-height 1))
            (lv-message
             "[RET] => prompt\n%s"
             (string-join (--map-indexed (format "[%d] => %s" it-index it) commands) "\n")))
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (define-key map (kbd "RET")
               (lambda!
                 (let ((compile-command (car commands)))
                   (lv-delete-window)
                   (call-interactively #'compile))))
             (define-key map (kbd "C-g")
               (lambda!
                 (lv-delete-window)
                 (call-interactively #'keyboard-quit)))
             (--map-indexed
              (define-key map (kbd (format "%d" it-index))
                (lambda!
                  (lv-delete-window)
                  (compile it)))
              commands)
             map)))))))

;;;###autoload
(defun ymacs-cpp/electric-star (-arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value -arg))
        (insert "  */")
        (backward-char 3)
        (indent-according-to-mode))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun ymacs-cpp/macro-expand ()
  (interactive)
  (when ymacs-cpp-expand-macro-function
    (setq-local c-macro-preprocessor
                (funcall ymacs-cpp-expand-macro-function)))
  (call-interactively 'c-macro-expand))

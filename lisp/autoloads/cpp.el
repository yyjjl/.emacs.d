;;; -*- lexical-binding: t; -*-

(defun cpp//compile-command (-dir)
  "Return suitable compile command for current project"
  (cond
   ((bound-and-true-p projectile-project-compilation-cmd)
    projectile-project-compilation-cmd)
   ((file-exists-p (expand-file-name "build.ninja" -dir))
    (concat "ninja -C " -dir))
   ((file-exists-p (expand-file-name "Makefile" -dir))
    (concat "make --no-print-directory -C " -dir))
   (t nil)))

;;;###autoload
(defun cpp/config-project (&optional -set-options)
  (interactive "P")
  (if (not cpp-cmake-project-root)
      (message "CMakeLists.txt hasn't been found.")
    (cpp-cmake//run-cmake-internal
     (lambda (buffer)
       (with-current-buffer buffer
         (when (bound-and-true-p lsp-enable-in-project-p)
           (let ((cdb-path (cpp-cmake//cdb-path)))
             (when (not (file-exists-p cdb-path))
               (error "Can not find compile_commands.json"))
             (let ((default-directory cpp-cmake-project-root))
               (make-symbolic-link cdb-path "compile_commands.json"
                                   :ok-if-already-exists)))
           (when cpp-has-ccls-p
             (cpp-ccls//setup))))))
    (when -set-options
      (cpp-cmake//set-cmake-options
       (lambda (_)
         (message "Finish setting cmake options"))))))

;;;###autoload
(defun cpp/load-file-in-root ()
  (interactive)
  (cond
   ((file-remote-p default-directory)
    (message "Not support in remove sever !"))
   ((not (executable-find "root"))
    (message "Executable `root' not found !"))
   ((not (buffer-file-name))
    (message "Buffer has no file !"))
   (t
    (let* ((file (buffer-file-name))
           (buffer-name (format "root:%s" (buffer-name)))
           (buffer (get-buffer-create buffer-name))
           (proc (get-buffer-process buffer)))
      (unless (and proc
                   (process-live-p proc)
                   (eq (buffer-local-value 'major-mode buffer)
                       'term-mode))
        (kill-buffer buffer)
        (setq buffer
              (term//exec-program "root" (list "-l" (or file "")) buffer-name)))
      (when buffer
        (with-current-buffer buffer
          (term-send-raw-string (format ".X %s\n" file)))
        (term//pop-to-buffer buffer))))))

;;;###autoload
(defun cpp/compile ()
  (interactive)
  (with-temp-env! (cpp-cmake//config-env)
    (-if-let (command (and cpp-cmake-project-root
                           (cpp//compile-command (cpp-cmake//config-build))))
        (compile command)
      (let ((compile-command
             (or (-when-let (dir (cpp-cmake//locate-cmakelists nil nil "Makefile"))
                   (cpp//compile-command dir))
                 (ignore-errors (cpp-ccls//buffer-compile-command))
                 compile-command)))
        (call-interactively 'compile)))))

;;;###autoload
(defun cpp/debug-current-file (&optional new-session)
  (interactive "P")
  (require 'realgud nil :noerror)
  (let ((cmd-bufs (cl-remove-if-not (lambda (x)
                                      (and (realgud-cmdbuf? x)
                                           (process-live-p (get-buffer-process x))))
                                    (buffer-list))))
    (if (or new-session (not cmd-bufs))
        (let ((default-directory
                (or (and current-prefix-arg default-directory)
                    (expand-file-name
                     (read-directory-name
                      "Directory: "
                      (file-name-as-directory (or (and cpp-cmake-project-root
                                                       (cpp-cmake//config-build))
                                                  default-directory))
                      nil
                      :must-match))))
              (gud-chdir-before-run nil))
          (realgud:gdb (or (ignore-errors (realgud:gdb-query-cmdline "gdb"))
                           (read-shell-command "Run like this: " nil
                                               'realgud:gdb-minibuffer-history))))
      (unless realgud-short-key-mode
        (realgud-short-key-mode 1)))))

;;;###autoload
(defun cpp/electric-star (-arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value -arg))
        (insert "  */")
        (backward-char 3)
        (indent-according-to-mode))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun cpp/begining-of-statment ()
  (interactive)
  (if (and (not current-prefix-arg)
           (or (= (char-before) ?\})
               (= (char-after) ?\})))
      (backward-sexp)
    (call-interactively 'c-beginning-of-statement)))

;;;###autoload
(defun cpp/end-of-statment ()
  (interactive)
  (if (and (not current-prefix-arg)
           (or (= (char-before) ?\{)
               (= (char-after) ?\{)))
      (forward-sexp)
    (call-interactively 'c-end-of-statement)))

(defvar c-macro-preprocessor)
;;;###autoload
(defun cpp/macro-expand ()
  (interactive)
  (setq-local c-macro-preprocessor
              (cpp-ccls//buffer-compile-command t))
  (call-interactively 'c-macro-expand))

(autoload 'cpp-cmake//render-config-buffer "init-cpp-cmake-ui")
;;;###autoload
(defun cpp-cmake/config ()
  (interactive)
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create "*cpp-cmake-config*")))
    (with-current-buffer (pop-to-buffer buffer)
      (cpp-cmake//render-config-buffer original-buffer))))

;;;###autoload
(defun cpp-cmake/change-config ()
  (interactive)
  (let ((name (completing-read
               "Change to: "
               (remove cpp-cmake-current-config-name
                       (mapcar #'car cpp-cmake-config-list))
               nil
               :require-match)))
    (unless (string= name cpp-cmake-current-config-name)
      (setq cpp-cmake-current-config-name name)
      (save-dir-local-variables! 'cpp-cmake-current-config-name)
      (cpp-cmake//run-cmake-internal))))

;;;###autoload
(defun cpp-cmake/toggle-option ()
  (interactive)
  (when-let* ((option (completing-read
                       "Options: "
                       (mapcar #'cpp-cmake//option-to-string
                               (--filter
                                (member (cdr it)
                                        '("Release" "Debug" "ON" "OFF"))
                                (cpp-cmake//config-options)))
                       nil
                       :require-match))
              (nv (split-string (substring option 2) "="))
              (option-name (car nv))
              (option-value (string-join (cdr nv)))
              (option (assoc option-name (cpp-cmake//config-options))))
    (setcdr option
            (cond ((equal "Release" option-value) "Debug")
                  ((equal "Debug" option-value) "Release")
                  ((equal "ON" option-value) "OFF")
                  ((equal "OFF" option-value) "ON")))
    (save-dir-local-variables! 'cpp-cmake-config-list)
    (cpp-cmake//run-cmake-internal)))

;;; -*- lexical-binding: t; -*-

(defun cpp-cmake//parse-available-options (-buffer)
  (with-current-buffer -buffer
    (goto-char (point-min))
    (print (buffer-string))
    (if (not (search-forward "-- Cache values" nil t))
        (user-error "Can not parse cmake output")
      (let (available-options)
        (while (re-search-forward
                "^\\([^:\n]+\\):\\([^=\n]+\\)=\\(.+\\)$" (point-max) t)
          (push (list (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3))
                available-options))
        (nreverse available-options)))))

(defun cpp-cmake//set-cmake-options (callback)
  (let* ((build-directory (cpp-cmake//config-build))
         (buffer (current-buffer))
         (default-directory (or build-directory default-directory)))
    (set-process-sentinel
     (with-temp-env! (cpp-cmake//config-env)
       (start-process "cmake" " *cmake*" "cmake" "-L" build-directory))
     (lambda (-proc _)
       (let* ((cmake-buffer (process-buffer -proc))
              (result (cpp-cmake//parse-available-options cmake-buffer)))
         (with-current-buffer buffer
           (setq cpp-cmake-available-options result)
           (save-dir-local-variables! 'cpp-cmake-available-options))
         (kill-buffer cmake-buffer))
       (funcall callback buffer)))))

(defun cpp-cmake//run-cmake-internal (&optional -callback)
  (cl-assert cpp-cmake-project-root nil "Not in a project")
  (let* ((build-directory (cpp-cmake//config-build))
         (buffer (current-buffer))
         (command (concat "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
                          (mapconcat #'cpp-cmake//option-to-string
                                     (cpp-cmake//config-options)
                                     " ")
                          " "
                          cpp-cmake-project-root))
         (default-directory build-directory))
    (when (not (file-exists-p (or build-directory "")))
      (if (yes-or-no-p (format "`%s' doesn't exist, create?" build-directory))
          (make-directory build-directory t)
        (user-error "Can not cd to build directory")))
    (with-current-buffer
        (with-temp-env! (cpp-cmake//config-env)
          (compilation-start command))
      (when -callback
        (add-transient-hook!
            (compilation-finish-functions
             :name cpp-cmake//run-cmake-sentinel
             :arguments (&rest _)
             :local t)
          (funcall -callback buffer))))))

(defun cpp//compile-command (-directory)
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
(defun cpp/config-project (&optional -set-options)
  (interactive "P")
  (if (not cpp-cmake-project-root)
      (message "CMakeLists.txt hasn't been found.")
    (cpp-cmake//run-cmake-internal
     (lambda (buffer)
       (with-current-buffer buffer
         (let ((cdb-path (cpp-cmake//cdb-path)))
           (when (not (file-exists-p cdb-path))
             (user-error "Can not find compile_commands.json"))
           (let ((default-directory cpp-cmake-project-root))
             (make-symbolic-link cdb-path "compile_commands.json"
                                 :ok-if-already-exists)))
         (cpp//main-setup))))
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
    (-if-let (command (when (and current-prefix-arg cpp-cmake-project-root)
                        (cpp//compile-command (cpp-cmake//config-build))))
        (compile command)
      (let ((commands
             (cl-remove
              nil
              `(,(when cpp-cmake-project-root
                   (cpp//compile-command (cpp-cmake//config-build)))
                ,(-when-let (dir (cpp-cmake//locate-cmakelists nil nil "Makefile"))
                   (cpp//compile-command dir))
                ,@(--filter
                   (stringp it)
                   (--map
                    (ignore-errors (funcall it))
                    cpp-buffer-command-functions))
                ,@(-take 2 compile-history)
                ,compile-command))))
        (let ((max-mini-window-height 1))
          (lv-message
           "%s"
           (concat
            "[RET] => prompt\n"
            (string-join (--map-indexed (format "[%d] => %s" it-index it) commands) "\n"))))
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "RET")
             (lambda! (let ((compile-command (car commands)))
                        (lv-delete-window)
                        (call-interactively #'compile))))
           (define-key map (kbd "C-g")
             (lambda! (lv-delete-window) (call-interactively #'keyboard-quit)))
           (--map-indexed
            (define-key map (kbd (format "%d" it-index))
              (lambda! (lv-delete-window) (compile it)))
            commands)
           map))))))

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

(defvar c-macro-preprocessor)
;;;###autoload
(defun cpp/macro-expand ()
  (interactive)
  (when cpp-expand-macro-function
    (setq-local c-macro-preprocessor (funcall cpp-expand-macro-function)))
  (call-interactively 'c-macro-expand))

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

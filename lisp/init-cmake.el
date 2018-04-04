(require-packages!
 cmake-mode
 cmake-font-lock)

(defcustom cpp-build-directory "build"
  "The build directory to run CMake in.  If nil, runs in a temp dir."
  :group 'cmake
  :type 'directory
  :safe #'stringp)

(defcustom cpp-cmake-options '(("CMAKE_BUILD_TYPE" . "Release"))
  "The cmake options of this project"
  :group 'cmake
  :type 'directory
  :safe #'listp)

(defvar-local cpp-cmakelists-directory nil)
(defvar cpp--cmake-process nil)

(defconst cpp--cquery-default-template
  "%gcc\n%c -std=gnu11\n%cpp -std=c++14\n\n")



(defsubst cpp%get-cmakelists ()
  (expand-file-name "CMakeLists.txt" cpp-cmakelists-directory))

(defsubst cpp%get-cdb-file ()
  (expand-file-name "compile_commands.json" cpp-build-directory))

(defun cpp%locate-cmakelists (&optional $dir $last-found $filename)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a
'plan B'."
  (let ((new-dir (locate-dominating-file (or $dir default-directory)
                                         (or $filename
                                             "CMakeLists.txt"))))
    (if new-dir
        (cpp%locate-cmakelists (expand-file-name ".." new-dir)
                                    new-dir
                                    $filename)
      $last-found)))

(defun cpp%get-compile-command ($dir)
  (cond
   ((bound-and-true-p projectile-project-compilation-cmd)
    projectile-project-compilation-cmd)
   ((file-exists-p (expand-file-name "build.ninja" $dir))
    (concat "ninja -C " $dir))
   ((file-exists-p (expand-file-name "Makefile" $dir))
    (concat "make --no-print-directory -C " $dir))
   (t nil)))

(defun cpp%maybe-create-build-directory ()
  (setq-local cpp-build-directory
              (expand-file-name (or cpp-build-directory "build")
                                cpp-cmakelists-directory))
  (or (file-exists-p cpp-build-directory)
      (make-directory cpp-build-directory)))

(defun cpp%maybe-create-cdb-file ()
  (when cpp-cmakelists-directory
    (let ((cdb-file (expand-file-name "compile_commands.json"
                                      cpp-cmakelists-directory)))
      (if (file-exists-p cdb-file)
          (cpp%cquery-setup)
        (cpp/run-cmake)
        (let ((default-directory cpp-cmakelists-directory))
          (ignore-errors
            (make-symbolic-link (cpp%get-cdb-file)
                                "compile_commands.json")))))))

(defun cpp%cmake-save-options (options)
  (when (add-dir-local-variable nil 'cpp-cmake-options options)
    (save-buffer)
    (bury-buffer))
  (dolist (buffer (projectile-project-buffers))
    (with-current-buffer buffer
      (hack-dir-local-variables-non-file-buffer))))

(defsubst cpp%option-to-string (option)
  (concat "-D" (car option) "=" (cdr option)))

(defsubst cpp%options-to-string ()
  (mapconcat #'cpp%option-to-string cpp-cmake-options " "))

(defun cpp/cmake-toggle-option ()
  (interactive)
  (when-let* ((option (completing-read
                       "Options: "
                       (mapcar #'cpp%option-to-string
                               (--filter (member (cdr it)
                                                 '("Release" "Debug" "ON" "OFF"))
                                         cpp-cmake-options))
                       nil
                       :require-match))
              (nv (split-string (substring option 2) "="))
              (option-name (car nv))
              (option-value (cadr nv)))
    (setcdr (assoc option-name cpp-cmake-options)
            (cond ((equal "Release" option-value) "Debug")
                  ((equal "Debug" option-value) "Release")
                  ((equal "ON" option-value) "OFF")
                  ((equal "OFF" option-value) "ON")))
    (cpp%cmake-save-options cpp-cmake-options)
    (cpp/run-cmake)))

(defun cpp/cmake-edit-option ()
  (interactive)
  (let* ((option-name (completing-read
                       "Options: "
                       (mapcar #'car cpp-cmake-options)))
         (nv (assoc option-name cpp-cmake-options))
         (option-value (read-string "Value: ")))
    (cond ((equal option-value "")
           ;; delete
           (setq cpp-cmake-options (delq nv cpp-cmake-options)))
          (nv (setcdr nv option-value))
          (t
           (push (cons option-name option-value) cpp-cmake-options)))
    (cpp%cmake-save-options cpp-cmake-options)
    (cpp/run-cmake)))

(defun cpp%run-cmake-internal (callback)
  (let ((default-directory (or cpp-build-directory default-directory)))
    (setq cpp--cmake-process
          (apply #'start-process "cmake" "*cmake*" "cmake"
                 "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                 (append (mapcar #'cpp%option-to-string cpp-cmake-options)
                         (list cpp-cmakelists-directory))))
    (lexical-let ((func callback))
      (set-process-sentinel cpp--cmake-process
                            (lambda ($process $event)
                              (message "Finished running CMake")
                              (funcall func))))))

(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map [f10] 'compile)
  (add-hook 'cmake-mode-hook
            (lambda ()
              (font-lock-mode 1)
              (cmake-font-lock-activate))))

(provide 'init-cmake)

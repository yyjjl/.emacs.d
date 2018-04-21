;; -*- lexical-binding:t -*-

(require-packages!
 cmake-mode
 cmake-font-lock)

(defcustom cpp-cmake-config-list
  '(("Release"
     (build . "build/release")
     (env . ())
     (options . (("CMAKE_BUILD_TYPE" . "Release"))))
    ("Debug"
     (build . "build/debug")
     (env . ())
     (options . (("CMAKE_BUILD_TYPE" . "Debug")))))
  "Project settings"
  :group 'cpp-cmake
  :type 'directory
  :safe #'listp)

(defcustom cpp-cmake-current-config-name "Release"
  "Project settings"
  :group 'cpp-cmake
  :type 'directory
  :safe #'stringp)

(defcustom cpp-cmake-available-options nil
  "All available cmake options of this project"
  :group 'cpp-cmake
  :type 'directory
  :safe #'listp)

(defvar-local cpp-cmake-project-root nil)



(defsubst cpp-cmake//current-config ()
  (assoc-string cpp-cmake-current-config-name cpp-cmake-config-list))

(defsubst cpp-cmake//config-name (&optional  config)
  (car (or config (cpp-cmake//current-config))))

(defsubst cpp-cmake//config-slot (slot &optional config)
  (cdr (assoc slot (cdr (or config (cpp-cmake//current-config))))))

(defsubst cpp-cmake//config-env (&optional config)
  (cpp-cmake//config-slot 'env config))

(defsubst cpp-cmake//config-options (&optional config)
  (cpp-cmake//config-slot 'options config))

(defsubst cpp-cmake//config-build (&optional config)
  (let ((item (assoc 'build (cdr (or config (cpp-cmake//current-config))))))
    (when item
      (setcdr item (expand-file-name (cdr item) cpp-cmake-project-root)))))

(defsubst cpp-cmake//set-config-slot (slot value &optional config)
  (setcdr (assoc slot (cdr (or config (cpp-cmake//current-config))))
          value))

(defsubst cpp-cmake//option-to-string (option)
  (concat "-D" (car option) "=" (cdr option)))

(defsubst cpp-cmake//new-config (name)
  (unless (assoc-string name cpp-cmake-config-list)
    (setq cpp-cmake-current-config-name name)
    (push `(,cpp-cmake-current-config-name
            (build . "build/Debug")
            (env . ())
            (options . (("CMAKE_BUILD_TYPE" . "Debug"))))
          cpp-cmake-config-list)))

(defsubst cpp-cmake//cmakelists-path ()
  (expand-file-name "CMakeLists.txt" cpp-cmake-project-root))

(defsubst cpp-cmake//cdb-path ()
  (expand-file-name "compile_commands.json" (cpp-cmake//config-build)))




(defun cpp-cmake//save-variables (&rest $variables)
  (save-window-excursion
    (dolist (var $variables)
      (add-dir-local-variable nil var
                              (buffer-local-value var (current-buffer))))
    (save-buffer)
    (dolist (buffer (or (ignore-errors (projectile-project-buffers))
                        (list (current-buffer))))
      (with-current-buffer buffer
        (hack-dir-local-variables-non-file-buffer)))))


(defun cpp-cmake//locate-cmakelists (&optional $dir $last-found $filename)
  "Find the topmost CMakeLists.txt."
  (let ((new-dir (locate-dominating-file (or $dir default-directory)
                                         (or $filename
                                             "CMakeLists.txt"))))
    (if new-dir
        (cpp-cmake//locate-cmakelists (expand-file-name ".." new-dir)
                               new-dir
                               $filename)
      $last-found)))

(defun cpp-cmake//run-cmake-internal (&optional $callback)
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
          (make-directory build-directory)
        (error "Can not cd to build directory")))
    (with-current-buffer (with-temp-env! (cpp-cmake//config-env)
                           (compilation-start command))
      (when $callback
        (add-hook 'compilation-finish-functions
                  (lambda (&rest _) (funcall $callback buffer))
                  nil
                  :local)))))

(defun cpp-cmake//parse-available-options ($buffer)
  (with-current-buffer $buffer
    (goto-char (point-min))
    (if (not (search-forward "-- Cache values" nil t))
        (error "Can not parse cmake output")
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
     (lambda ($proc _)
       (let* ((cmake-buffer (process-buffer $proc))
              (result (cpp-cmake//parse-available-options cmake-buffer)))
         (with-current-buffer buffer
           (setq cpp-cmake-available-options result)
           (cpp-cmake//save-variables 'cpp-cmake-available-options))
         (kill-buffer cmake-buffer))
       (funcall callback buffer)))))



(autoload 'cpp-cmake//render-config-buffer "init-cpp-cmake-ui")

(defun cpp-cmake/config ()
  (interactive)
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create "*cpp-cmake-config*")))
    (with-current-buffer (pop-to-buffer buffer)
      (cpp-cmake//render-config-buffer original-buffer))))

(defun cpp-cmake/change-config ()
  (interactive)
  (let ((name (completing-read "Config: "
                               (mapcar #'car cpp-cmake-config-list)
                               nil
                               :require-match)))
    (unless (string= name cpp-cmake-current-config-name)
      (setq cpp-cmake-current-config-name name)
      (cpp-cmake//save-variables 'cpp-cmake-current-config-name)
      (cpp-cmake//run-cmake-internal))))

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
    (cpp-cmake//save-variables 'cpp-cmake-config-list)
    (cpp-cmake//run-cmake-internal)))

(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map [f10] 'compile)
  (add-hook 'cmake-mode-hook
            (lambda ()
              (font-lock-mode 1)
              (cmake-font-lock-activate))))

(provide 'init-cpp-cmake)

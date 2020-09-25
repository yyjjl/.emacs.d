;;; -*- lexical-binding: t; -*-

(defun ymacs-cpp-cmake//parse-available-options (-buffer)
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

(defun ymacs-cpp-cmake//set-cmake-options (-callback)
  (let* ((buffer (current-buffer))
         (build-directory (ymacs-cpp-cmake//config-build))
         (default-directory (or build-directory default-directory)))
    (set-process-sentinel

     (with-temp-env! (ymacs-cpp-cmake//config-env)
       (start-process "cmake" " *cmake*" "cmake" "-L" build-directory))

     (lambda (-proc _)
       (let* ((cmake-buffer (process-buffer -proc))
              (result (ymacs-cpp-cmake//parse-available-options cmake-buffer)))
         (with-current-buffer buffer
           (setq ymacs-cpp-cmake-available-options result)
           (save-dir-local-variables! 'ymacs-cpp-cmake-available-options))
         (kill-buffer cmake-buffer))

       (funcall -callback buffer)))))

(defun ymacs-cpp-cmake//run-cmake (&optional -callback)
  (unless ymacs-cpp-cmake-project-root
    (user-error "Not in a CMake project"))

  (let* ((buffer (current-buffer))
         (build-directory (ymacs-cpp-cmake//config-build))
         (command (concat "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
                          (mapconcat #'ymacs-cpp-cmake//option-to-string
                                     (ymacs-cpp-cmake//config-options)
                                     " ")
                          " "
                          ymacs-cpp-cmake-project-root))
         (default-directory build-directory))

    (when (not (file-exists-p (or build-directory "")))
      (if (yes-or-no-p (format "`%s' doesn't exist, create?" build-directory))
          (make-directory build-directory t)
        (user-error "Can not cd to build directory")))

    (with-current-buffer
        (with-temp-env! (ymacs-cpp-cmake//config-env)
          (compilation-start command))

      (when -callback
        (add-transient-hook!
            (compilation-finish-functions
             :name ymacs-cpp-cmake//run-cmake-sentinel
             :arguments (&rest _)
             :local t)
          (funcall -callback buffer))))))

;;;###autoload
(defun ymacs-cpp-cmake/config-project (&optional -set-options)
  (interactive "P")
  (if (not ymacs-cpp-cmake-project-root)
      (message "CMakeLists.txt hasn't been found.")

    (ymacs-cpp-cmake//run-cmake
     (lambda (buffer)
       (with-current-buffer buffer
         (let ((cdb-path (ymacs-cpp-cmake//cdb-path)))
           (when (not (file-exists-p cdb-path))
             (user-error "Can not find compile_commands.json"))

           (let ((default-directory ymacs-cpp-cmake-project-root))
             (make-symbolic-link cdb-path
                                 "compile_commands.json"
                                 :ok-if-already-exists)))
         (ymacs-cpp//cpp-setup))))

    (when -set-options
      (ymacs-cpp-cmake//set-cmake-options
       (lambda (_)
         (message "Finish setting cmake options"))))))

;;;###autoload
(defun ymacs-cpp-cmake/change-config ()
  (interactive)
  (let ((name (completing-read
               "Change to: "
               (remove ymacs-cpp-cmake-current-config-name
                       (mapcar #'car ymacs-cpp-cmake-config-list))
               nil
               :require-match)))
    (unless (string= name ymacs-cpp-cmake-current-config-name)
      (setq ymacs-cpp-cmake-current-config-name name)
      (save-dir-local-variables! 'ymacs-cpp-cmake-current-config-name)
      (ymacs-cpp-cmake//run-cmake))))

;;;###autoload
(defun ymacs-cpp-cmake/toggle-option ()
  (interactive)
  (when-let*
      ((option (completing-read
                "Options: "
                (mapcar #'ymacs-cpp-cmake//option-to-string
                        (--filter
                         (member (cdr it)
                                 '("Release" "Debug" "ON" "OFF"))
                         (ymacs-cpp-cmake//config-options)))
                nil
                :require-match))
       (nv (split-string (substring option 2) "="))
       (option-name (car nv))
       (option-value (string-join (cdr nv)))
       (option (assoc option-name (ymacs-cpp-cmake//config-options))))
    (setcdr option
            (cond ((equal "Release" option-value) "Debug")
                  ((equal "Debug" option-value) "Release")
                  ((equal "ON" option-value) "OFF")
                  ((equal "OFF" option-value) "ON")))
    (save-dir-local-variables! 'ymacs-cpp-cmake-config-list)
    (ymacs-cpp-cmake//run-cmake)))

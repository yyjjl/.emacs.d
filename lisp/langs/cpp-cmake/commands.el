;;; -*- lexical-binding: t; -*-

(autoload 'ymacs-cpp-cmake//render-config-buffer (expand! "commands-ui"))

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

(defun ymacs-cpp-cmake//set-cmake-options--callback (-source-buffer)
  (let ((command-buffer (current-buffer)))
    (with-current-buffer -source-buffer
      (setq ymacs-cpp-cmake-available-options
            (ymacs-cpp-cmake//parse-available-options command-buffer))
      (save-dir-local-variables! 'ymacs-cpp-cmake-available-options)

      (when-let (config-ui-buffer (get-buffer ymacs-cpp-cmake-config-buffer-name))
        (with-current-buffer config-ui-buffer
          (ymacs-cpp-cmake//render-config-buffer -source-buffer)))

      (message "Finish setting cmake options"))
    (kill-buffer command-buffer)))

(defun ymacs-cpp-cmake//set-cmake-options ()
  (let* ((buffer (current-buffer))
         (build-directory (ymacs-cpp-cmake//config-build))
         (default-directory (or build-directory default-directory)))
    (run-process!
     :-name "cmake"
     :-program-args (list "-L" build-directory)
     :-callback
     (lambda (_)
       (ymacs-cpp-cmake//set-cmake-options--callback buffer)))))

(defun ymacs-cpp-cmake//run-cmake (&optional -callback)
  (unless ymacs-cpp-cmake-project-root
    (user-error "CMakeLists.txt hasn't been found."))

  (let* ((buffer (current-buffer))
         (build-directory (ymacs-cpp-cmake//config-build))
         (command (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s %s"
                          (mapconcat #'ymacs-cpp-cmake//option-to-string
                                     (ymacs-cpp-cmake//config-options)
                                     " ")
                          ymacs-cpp-cmake-project-root))
         (default-directory build-directory))

    (when (not (file-exists-p (or build-directory "")))
      (if (yes-or-no-p (format "`%s' doesn't exist, create?" build-directory))
          (make-directory build-directory t)
        (user-error "Can not cd to build directory")))

    (run-compilation!
     :-buffer-name (format "*CMake Config: %s*" ymacs-cpp-cmake-project-root)
     :-command command
     :-callback (when -callback
                  (lambda (&rest _) (funcall -callback buffer))))))

(defun ymacs-cpp-cmake//config-project--callback (-source-buffer)
  (with-current-buffer -source-buffer
    (let ((cdb-path (ymacs-cpp-cmake//cdb-path)))
      (when (not (file-exists-p cdb-path))
        (user-error "Can not find compile_commands.json"))

      (let ((default-directory ymacs-cpp-cmake-project-root))
        (make-symbolic-link cdb-path "compile_commands.json" :ok-if-already-exists)))
    (ymacs-cpp//cpp-setup)
    (hack-dir-local-variables-non-file-buffer)
    (ymacs-cpp-cmake//set-cmake-options)))

;;;###autoload
(defun ymacs-cpp-cmake/config-project ()
  (interactive)
  (ymacs-cpp-cmake//run-cmake #'ymacs-cpp-cmake//config-project--callback))

;;;###autoload
(defun ymacs-cpp-cmake/change-config ()
  (interactive)

  (setq ymacs-cpp-cmake-current-config-name
        (completing-read-simple!
         :-prompt "Change to: "
         :-collection (remove ymacs-cpp-cmake-current-config-name
                              (mapcar #'car ymacs-cpp-cmake-config-list))))
  (save-dir-local-variables! 'ymacs-cpp-cmake-current-config-name)
  (ymacs-cpp-cmake//run-cmake))

;;;###autoload
(defun ymacs-cpp-cmake/toggle-option ()
  (interactive)
  (when-let ((options (cl-loop
                       for (key . value) in (ymacs-cpp-cmake//config-options)
                       when (member value '("Release" "Debug" "ON" "OFF"))
                       collect (ymacs-cpp-cmake//option-to-string (cons key value))))

             (option (completing-read-simple!
                      :-prompt "Options: "
                      :-collection options))

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

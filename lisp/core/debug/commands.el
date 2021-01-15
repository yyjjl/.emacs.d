;;; -*- lexical-binding: t; -*-

(defvar gud-chdir-before-run)

;;;###autoload
(defun ymacs-debug/quit ()
  (interactive)
  (and (yes-or-no-p "Quit debug session?")
       (gud-call "quit")))

;;;###autoload
(defun ymacs-debug/debug-current-file (&optional -select-directory)
  (interactive)
  (let* ((debugger
          (or (alist-get major-mode ymacs-debugger-alist)
              (user-error "No entry in `ymacs-debugger-alist' for %s" major-mode)))
         (debug-fn (car debugger))
         (debug-args (cdr debugger)))
    (if (plist-get debug-args :gud)
        ;;  gud debugger
        (unless (ymacs-debug//resuse-session)
          (let* ((directory (ymacs-term//get-directory))
                 (default-directory
                   (or (when -select-directory
                         (thread-first directory
                           (or default-directory)
                           file-name-as-directory
                           (read-directory-name nil :must-match)))
                       directory
                       default-directory))
                 (gud-chdir-before-run nil))
            (with-temp-lv-message!
                ("Current directory: %s" default-directory)
              (call-interactively debug-fn))))
      (call-interactively debug-fn))))

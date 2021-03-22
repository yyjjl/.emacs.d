;;; -*- lexical-binding: t; -*-

(autoload 'desktop-full-file-name "desktop")
(autoload 'desktop-kill "desktop")

(defsubst ymacs-desktop//desktop-files ()
  (cl-loop
   for directory in (directory-files ymacs-desktop-directory nil "\\`[^\\.]")
   when (file-exists-p (desktop-full-file-name (expand-file-name directory ymacs-desktop-directory)))
   collect directory))

;;;###autoload
(defsubst ymacs-desktop//current-desktop ()
  (when desktop-dirname
    (string-trim-right (file-relative-name desktop-dirname ymacs-desktop-directory) "/")))

;;;###autoload
(defun ymacs-desktop/change-or-new-desktop (-name)
  (interactive
   (list (completing-read (format "Change to (current %s): " (ymacs-desktop//current-desktop))
                          (ymacs-desktop//desktop-files))))

  (let ((new-dir (expand-file-name -name ymacs-desktop-directory))
        (old-name (ymacs-desktop//current-desktop)))
    (when (and old-name
               (yes-or-no-p (format "save desktop '%s'? " old-name)))
      (desktop-save desktop-dirname))
    (if (file-exists-p new-dir)
        (if (not (and desktop-dirname
                      (equal (file-name-as-directory new-dir)
                             (file-name-as-directory desktop-dirname))
                      (file-exists-p (desktop-full-lock-name))))
            (when (yes-or-no-p (format "Change to desktop '%s' (current '%s')? " -name old-name))
              (ymacs-desktop//without-semantic-mode
                (desktop-change-dir new-dir)))
          (user-error "Desktop file is in use !!"))
      (make-directory new-dir t)
      (desktop-kill)
      (desktop-save new-dir)))

  (setq ymacs-desktop-name
        (when-let (name (ymacs-desktop//current-desktop))
          (format " <%s>" name))))

;;;###autoload
(defun ymacs-desktop/delete-desktop (-name)
  (interactive
   (list (completing-read "Delete: " (ymacs-desktop//desktop-files) nil :require-match)))

  (let* ((dir (expand-file-name -name ymacs-desktop-directory)))
    (when (and (file-exists-p dir)
               (yes-or-no-p (format "Delete desktop `%s'?" -name)))
      (when (equal (file-name-as-directory dir)
                   (file-name-as-directory desktop-dirname))
        (ymacs-desktop/change-or-new-desktop "default")
        (message "Change to default desktop."))
      (delete-directory dir t))))

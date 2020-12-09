;; -*- lexical-binding:t -*-

(autoload 'desktop-full-lock-name "desktop")
(autoload 'desktop-kill "desktop")

(defsubst ymacs-tools//desktop-files ()
  (--remove
   (member it '("." ".."))
   (--map (file-name-nondirectory it)
          (--filter (file-exists-p (file-name-as-directory it))
                    (directory-files (expand-var! "desktop") :full)))))
;;;###autoload
(defun ymacs-tools/change-or-new-desktop (-name)
  (interactive
   (list (completing-read "Change to: "
                          (cons "default" (ymacs-tools//desktop-files)))))
  (let ((new-dir (expand-file-name (if (equal -name "default")
                                       ""
                                     -name)
                                   (expand-var! "desktop"))))
    (if (file-exists-p new-dir)
        (if (not (and desktop-dirname
                      (equal (file-name-as-directory new-dir)
                             (file-name-as-directory desktop-dirname))
                      (file-exists-p (desktop-full-lock-name))))
            (when (y-or-n-p (format "Change to desktop '%s' (current '%s')? "
                                    -name
                                    ymacs-tools-current-desktop-name))
              (semantic-mode -1)
              (setq ymacs-tools-current-desktop-name -name)
              (desktop-change-dir new-dir)
              (semantic-mode 1))
          (user-error "Desktop file is in use !!"))
      (make-directory new-dir t)
      (desktop-kill)
      (desktop-save new-dir))))

;;;###autoload
(defun ymacs-tools/delete-desktop (-name)
  (interactive (list (completing-read "Delete: "
                                      (ymacs-tools//desktop-files)
                                      nil
                                      :require-match)))
  (let* ((default (expand-var! "desktop"))
         (dir (expand-file-name -name default)))
    (when (and (file-exists-p dir)
               (not (equal default dir))
               (y-or-n-p (format "Delete desktop `%s'?" -name)))
      (when (equal (file-name-as-directory dir)
                   (file-name-as-directory desktop-dirname))
        (progn (desktop-change-dir default)
               (setq ymacs-tools-current-desktop-name "default")
               (message "Change to default desktop.")))
      (delete-directory dir t))))

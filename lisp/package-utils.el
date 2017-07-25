(require 'package)
(require 'subr-x)
(eval-when-compile
  (require 'cl))

(defvar package|use-priority-p nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package|priority-alist nil "Package archive priority.")
(defvar package|required-packages (make-hash-table) "All packages required.")

(defun package|autoclose-autoloads (name pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
              (expand-file-name (concat (if (symbolp name)
                                            (symbol-name name)
                                          name)
                                        "-autoloads.el")
                                pkg-dir))))
    (when buf (kill-buffer buf))))

(defun package|add (pkgs &optional archive)
  (dolist (pkg pkgs)
    (puthash pkg archive package|required-packages)))

(defsubst package|archive-priority (archive)
  (length (member (package-desc-archive archive) package|priority-alist)))

(defun package|get-archive (name archives)
  (unless (null archives)
    (if (string= name (package-desc-archive (car archives)))
        (car archives)
      (package|get-archive name (cdr archives)))))

(defun package|set-archive (pkg &optional archive-name)
  "Set right archive content for PKG."
  (let ((pkg-archives (cdr pkg)) archive)
    (when archive-name
      (setq archive (package|get-archive archive-name pkg-archives)))
    (when (and (not archive) package|use-priority-p)
      (setq archive (car (sort pkg-archives
                               (lambda (pkg1 pkg2)
                                 (> (package|archive-priority pkg1)
                                    (package|archive-priority pkg2)))))))
    (when archive
      (setf (cdr pkg) (list archive)))))

(defun package|after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package|set-archive pkg (gethash (car pkg) package|required-packages))))

(defun package|load-all ()
  "Load Emacs Lisp packages."
  (when package|use-priority-p
    (setq package|priority-alist (mapcar #'car package-archives)))
  (advice-add 'package-generate-autoloads
              :after #'package|autoclose-autoloads)
  (advice-add 'package-read-all-archive-contents
              :after #'package|after-read-contents)

  (message "Loading packages ...")
  (package-initialize 'no-activate)
  (when (>= emacs-major-version 25)
    ;; Do not save to init.el
    (fset 'package--save-selected-packages
          (lambda (value)
            (when value
              (setq package-selected-packages value))))
    (setq package-selected-packages (hash-table-keys package|required-packages)))

  (let (freshed-p)
    (maphash (lambda (pkg _)
               (unless (package-installed-p pkg)
                 (unless freshed-p
                   (setq freshed-p t)
                   (package-refresh-contents))
                 (message "Installing package '%s' ..." pkg)
                 (package-install pkg)
                 (message "Package'%s' installed" pkg)))
             package|required-packages))

  (dolist (e package-alist)
    (package-activate (car e))))

(provide 'package-utils)

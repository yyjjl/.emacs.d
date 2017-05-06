(require 'package)
(require 'cl)

(defcustom package-use-priority nil
  "Non-nil means to use priority defined in variable `package-archive-priority'.
Archive with high priority will be used when install a package."
  :group 'package-utils)

(defcustom package-autoclose-autoloads t
  "Auto close *-autoloads.el after a package installed."
  :group 'package-utils)

(defvar package-archive-priority-alist nil "Package archive priority.")
(defvar required-packages (make-hash-table) "All packages required.")

(defun autoclose-autoloads (name pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
              (expand-file-name (concat (if (symbolp name)
                                            (symbol-name name)
                                          name)
                                        "-autoloads.el") pkg-dir))))
    (when buf (kill-buffer buf))))

(defun add-packages (pkgs &optional archive)
  (dolist (pkg pkgs)
    (puthash pkg archive required-packages)))

(defsubst max-of (seq &optional key)
  (let* ((func (or key #'identity))
         (value (car seq))
         (maximum (funcall func value)))
    (when value
      (dolist (elt (cdr seq))
        (let ((m (funcall func elt)))
          (when (< maximum m)
            (setq maximum m
                  value elt)))))
    value))

(defsubst package-archive-priority (archive)
  (length (member (package-desc-archive archive)
                  package-archive-priority-alist)))

(defun package-set-right-archive (pkg &optional archive-name)
  "Set right archive content by priority."
  (let* ((pkg-archives (cdr pkg)) archive)
    (when archive-name
      (setq archive (find archive-name pkg-archives
                          :test (lambda (name info)
                                  (string= name (package-desc-archive info))))))
    (when (and (not archive) package-use-priority)
      (setq archive (max-of pkg-archives #'package-archive-priority)))
    (when archive
      (setf (cdr pkg) (list archive)))))

(defun require-packages ()
  (let (freshed-p)
    (maphash (lambda (pkg _)
               (unless (package-installed-p pkg)
                 (unless freshed-p
                   (setq freshed-p t)
                   (package-refresh-contents))
                 (package-install pkg)))
             required-packages)))


(defun manager-packages-with-priority ()
  (dolist (pkg package-archive-contents)
    (package-set-right-archive pkg (gethash (car pkg) required-packages))))

(defun package-utils-initialize (&optional no-activate)
  "Load Emacs Lisp packages."
  (when package-use-priority
    (setq package-archive-priority-alist (mapcar #'car package-archives)))
  (when package-autoclose-autoloads
    (advice-add 'package-generate-autoloads :after #'autoclose-autoloads))
  (advice-add 'package-read-all-archive-contents :after
              #'manager-packages-with-priority)

  (package-initialize 'no-activate)

  (require-packages)

  (dolist (elt package-alist)
    (package-activate (car elt))))

(provide 'package-utils)
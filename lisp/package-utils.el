(require 'package)
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
                                        "-autoloads.el") pkg-dir))))
    (when buf (kill-buffer buf))))

(defun package|max-of (seq &optional key)
  "Get max element of SEQ.
KEY is a function to compute value of element."
  (unless key (setq key #'identity))
  (let* ((value (car seq))
         (maximum (funcall key value)))
    (when value
      (dolist (e (cdr seq))
        (let ((m (funcall key e)))
          (when (< maximum m)
            (setq maximum m
                  value e)))))
    value))

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
      (setq archive (package|max-of pkg-archives #'package|archive-priority)))
    (when archive
      (setf (cdr pkg) (list archive)))))

(defun package|load-all ()
  (let (freshed-p)
    (maphash (lambda (pkg _)
               (unless (package-installed-p pkg)
                 (unless freshed-p
                   (setq freshed-p t)
                   (package-refresh-contents))
                 (package-install pkg)))
             package|required-packages)))


(defun package|after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package|set-archive pkg (gethash (car pkg) package|required-packages))))

(defun package|initialize (&optional no-activate)
  "Load Emacs Lisp packages."
  (when package|use-priority-p
    (setq package|priority-alist (mapcar #'car package-archives)))
  (advice-add 'package-generate-autoloads
              :after #'package|autoclose-autoloads)
  (advice-add 'package-read-all-archive-contents
              :after #'package|after-read-contents)

  (package-initialize 'no-activate)

  (package|load-all)

  (dolist (e package-alist)
    (package-activate (car e))))

(provide 'package-utils)
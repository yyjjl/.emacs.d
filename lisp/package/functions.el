;; -*- lexical-binding: t; -*-

(defsubst ymacs-package//archive-priority (-archive)
  (length (member (package-desc-archive -archive) ymacs-package--priority-alist)))

(defun ymacs-package//get-archive (-name -archives)
  (let (archive tmp)
    (while (and -archives (not archive))
      (setq tmp (pop -archives))
      (when (string= -name (package-desc-archive tmp))
        (setq archive tmp)))
    archive))

(defun ymacs-package//set-archive (-pkg &optional -archive-name)
  "Set right archive content for PKG. "
  (let ((pkg-archives (cdr -pkg)) archive)
    (when -archive-name
      (setq archive (ymacs-package//get-archive -archive-name pkg-archives)))
    (when (and (not archive) ymacs-package--use-priority-p)
      (setq archive (car (sort pkg-archives
                               (lambda (-pkg1 -pkg2)
                                 (> (ymacs-package//archive-priority -pkg1)
                                    (ymacs-package//archive-priority -pkg2)))))))
    (when archive
      (setf (cdr -pkg) (list archive)))))

(defun ymacs-pacakge//init ()
  (setq package-selected-packages
        (hash-table-keys ymacs-package--required-packages))

  (unless (file-exists-p ymacs-autoloads-file)
    (autoload 'ymacs-package/generate-autoloads "package/commands" nil t)
    (ymacs-package/generate-autoloads))

  (load ymacs-autoloads-file))

(defun require! (-pkg-name &optional -archive -location)
  (cond ((eq -location 'built-in) t)
        (-location (add-to-list 'load-path -location))
        (t
         (puthash -pkg-name -archive ymacs-package--required-packages)
         (unless (package-installed-p -pkg-name)
           (unless ymacs-package--content-freshed-p
             (package-refresh-contents)
             (setq ymacs-package--content-freshed-p t))
           (message "Installing package `%s' ..." -pkg-name)
           (let ((inhibit-message t))
             (package-install -pkg-name))))))

(defmacro require-packages! (&rest -pkg-list)
  (declare (indent nil))
  (let (forms compile-forms)
    (dolist (pkg -pkg-list)
      (if (atom pkg)
          (progn (push `(require! ',pkg) forms)
                 (push `(require ',pkg) compile-forms))
        (let* ((when-form (plist-get (cdr pkg) :when))
               (form `(require! ',(car pkg)
                                ,(plist-get (cdr pkg) :archive)
                                ,(plist-get (cdr pkg) :location)))
               (pkgs-required-when-compile (or (plist-get (cdr pkg) :compile)
                                               (list (car pkg))))
               (compile-form (mapcar (lambda (x)
                                       `(require ',x))
                                     pkgs-required-when-compile)))
          (if when-form
              (progn
                (push `(when ,when-form ,form) forms)
                (push `(when ,when-form ,@compile-form) compile-forms))
            (push form forms)
            (setq compile-forms
                  (nconc (nreverse compile-form) compile-forms))))))
    `(progn
       ,@(nreverse forms)
       (ignore-errors
         (eval-when-compile
           ,@(nreverse compile-forms))))))

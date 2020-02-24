(require 'package)

(advice-add
 'package--save-selected-packages
 :override
 (lambda (-value)
   (when -value
     (setq package-selected-packages -value))
   (unless after-init-time
     (add-hook 'after-init-hook #'package--save-selected-packages))))

(defvar package--use-priority-p nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package--priority-alist nil "Package archive priority.")

(defvar package--required-packages (make-hash-table)
  "All packages required.")

;; The index of archive represents its priority
(setq package-archives
      '(("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; Setup to select right archive
(setq package--priority-alist (mapcar #'car package-archives))

(unless emacs-use-gnutls-p
  (dolist (item package-archives)
    (setcdr item (concat "http://" (string-trim-left (cdr item) "https://")))))

(defun package*autoclose-autoloads (-name -pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
               (expand-file-name (concat (if (symbolp -name)
                                             (symbol-name -name)
                                           -name)
                                         "-autoloads.el")
                                 -pkg-dir))))
    (when buf (kill-buffer buf))))
(advice-add 'package-generate-autoloads :after #'package*autoclose-autoloads)

(defsubst package//archive-priority (-archive)
  (length (member (package-desc-archive -archive) package--priority-alist)))

(defun package//get-archive (-name -archives)
  (let (archive tmp)
    (while (and -archives (not archive))
      (setq tmp (pop -archives))
      (when (string= -name (package-desc-archive tmp))
        (setq archive tmp)))
    archive))

(defun package//set-archive (-pkg &optional -archive-name)
  "Set right archive content for PKG. "
  (let ((pkg-archives (cdr -pkg)) archive)
    (when -archive-name
      (setq archive (package//get-archive -archive-name pkg-archives)))
    (when (and (not archive) package--use-priority-p)
      (setq archive (car (sort pkg-archives
                                (lambda (-pkg1 -pkg2)
                                  (> (package//archive-priority -pkg1)
                                     (package//archive-priority -pkg2)))))))
    (when archive
      (setf (cdr -pkg) (list archive)))))

(defun package*after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package//set-archive pkg (gethash (car pkg)
                                       package--required-packages))))
(advice-add 'package-read-all-archive-contents
            :after #'package*after-read-contents)

(defvar package--content-freshed-p nil)
(defun require! (-pkg-name &optional -archive -location)
  (cond ((eq -location 'built-in) t)
        (-location (add-to-list 'load-path -location))
        (t
         (puthash -pkg-name -archive package--required-packages)
         (unless (package-installed-p -pkg-name)
           (unless package--content-freshed-p
             (package-refresh-contents)
             (setq package--content-freshed-p t))
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

(if (bound-and-true-p core-dumped)
    (setq load-path core-dumped-load-path)
  ;; add load-path’s and load autoload files
  (package-initialize))

(unless (file-exists-p emacs-autoloads-file)
  (package/generate-autoloads))

(load emacs-autoloads-file)

(provide 'core-packages-lib)

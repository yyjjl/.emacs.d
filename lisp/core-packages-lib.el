(require 'package)

(when (>= emacs-major-version 25)
  ;; Do not save to init.el
  (fset 'package--save-selected-packages
        (lambda ($value)
          (when $value (setq package-selected-packages $value)))))

(defvar package--use-priority-p nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package--priority-alist nil "Package archive priority.")

(defvar package--required-packages (make-hash-table)
  "All packages required.")

;; The index of archive represents its priority
(setq package-archives
      (if (gnutls-available-p)
          '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
            ("melpa" . "https://elpa.emacs-china.org/melpa/")
            ("org" . "https://orgmode.org/elpa/")
            ("gnu" . "https://elpa.emacs-china.org/gnu/"))
        '(("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
          ("melpa" . "http://elpa.emacs-china.org/melpa/")
          ("org" . "http://orgmode.org/elpa/")
          ("gnu" . "http://elpa.emacs-china.org/gnu/"))))
;; (setq package-archives
;;       (if (gnutls-available-p)
;;           '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;             ("melpa" . "https://melpa.org/packages/")
;;             ("org" . "https://orgmode.org/elpa/")
;;             ("gnu" . "https://elpa.gnu.org/packages/"))
;;         '(("melpa-stable" . "http://stable.melpa.org/packages/")
;;           ("melpa" . "http://melpa.org/packages/")
;;           ("org" . "http://orgmode.org/elpa/")
;;           ("gnu" . "http://elpa.gnu.org/packages/"))))

;; Setup to select right archive
(setq package--priority-alist (mapcar #'car package-archives))

(defun package*autoclose-autoloads ($name $pkg-dir)
  "Auto close *-autoloads.el after a package installed."
  (let ((buf (find-file-existing
               (expand-file-name (concat (if (symbolp $name)
                                             (symbol-name $name)
                                           $name)
                                         "-autoloads.el")
                                 $pkg-dir))))
    (when buf (kill-buffer buf))))
(advice-add 'package-generate-autoloads :after #'package*autoclose-autoloads)

(defsubst package//archive-priority ($archive)
  (length (member (package-desc-archive $archive) package--priority-alist)))

(defun package//get-archive ($name $archives)
  (let (archive tmp)
    (while (and $archives (not archive))
      (setq tmp (pop $archives))
      (when (string= $name (package-desc-archive tmp))
        (setq archive tmp)))
    archive))

(defun package//set-archive ($pkg &optional $archive-name)
  "Set right archive content for PKG. "
  (let ((pkg-archives (cdr $pkg)) archive)
    (when $archive-name
      (setq archive (package//get-archive $archive-name pkg-archives)))
    (when (and (not archive) package--use-priority-p)
      (setq archive (car (sort pkg-archives
                                (lambda ($pkg1 $pkg2)
                                  (> (package//archive-priority $pkg1)
                                     (package//archive-priority $pkg2)))))))
    (when archive
      (setf (cdr $pkg) (list archive)))))

(defun package*after-read-contents ()
  (dolist (pkg package-archive-contents)
    (package//set-archive pkg (gethash (car pkg)
                                       package--required-packages))))
(advice-add 'package-read-all-archive-contents
            :after #'package*after-read-contents)

(defvar package--content-freshed-p nil)
(defun require! ($pkg-name &optional $archive $location)
  (cond ((eq $location 'built-in) t)
        ($location (add-to-list 'load-path $location))
        (t
         (puthash $pkg-name $archive package--required-packages)
         (unless (package-installed-p $pkg-name)
           (unless package--content-freshed-p
             (package-refresh-contents)
             (setq package--content-freshed-p t))
           (message "Installing package `%s' ..." $pkg-name)
           (let ((inhibit-message t))
             (package-install $pkg-name))))))

(defmacro require-packages! (&rest $pkg-list)
  (declare (indent nil))
  (let (forms compile-forms)
    (dolist (pkg $pkg-list)
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

(defun package/generate-autoloads ()
  (interactive)
  (require 'autoload)
  (let ((files (directory-files emacs-autoloads-directory :full "\\.el\\'"))
        (failed-count 0))
    (with-current-buffer (find-file-noselect emacs-autoloads-file)
      (erase-buffer)
      (dolist (file files)
        (condition-case nil
            (generate-file-autoloads file)
          (error (message "Generating for %s failed" file)
                 (incf failed-count))))
      (save-buffer))
    (message "%d generated, %d failed"
             (- (length files) failed-count)
             failed-count)))

(defun package/compile-elpa-packages (&optional $no-message?)
  (interactive)
  (let ((inhibit-message $no-message?))
    (byte-recompile-directory package-user-dir nil :force)))

(defun package/compile-config (&optional $no-message?)
  (interactive "P")
  (message "Compile configuration files ...")
  (dolist (file (append
                 (directory-files emacs-config-directory :full "\\.el$")
                 (directory-files-recursively emacs-private-directory "\\.el$")
                 (list user-init-file
                       custom-file)))
    (when file
      (condition-case err
          (let ((inhibit-message $no-message?))
            (byte-compile-file file))
        (error (message "Error: %s" err)
               (backtrace)))))
  (message "Compile finished"))

(package-initialize)

(unless (file-exists-p emacs-autoloads-file)
  (package/generate-autoloads))
(load emacs-autoloads-file)

(provide 'core-packages-lib)

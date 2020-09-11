;; -*- lexical-binding: t; -*-

(defvar package--content-freshed-p nil)

(defvar package--use-priority-p nil
  "Non-nil means to use priority defined in variable `package|priority-alist'.
Archive with high priority will be used when install a package.")

(defvar package--priority-alist nil "Package archive priority.")

(defvar package--required-packages (make-hash-table)
  "All packages required.")

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

(defvar core-config-keywords-alist
  '((:init t :order -2)
    (:bind core//config-binds :order 1)
    (:hook core//config-hooks :order 2)
    (:advice core//config-advices :order 3)
    (:config t :order 4)))

(defun core//config-binds (_prefix -binds)
  (cl-loop
   for form in -binds
   collect `(define-key! ,@form)))

(defun core//config-hooks (-prefix -hooks)
  (cl-loop
   for (symbol . args) in -hooks
   append
   (if (null (cdr args))
       (mapcar (lambda (hook)
                 (let ((name (if (eq :use (car-safe symbol))
                                 (intern (format "%s|%s" -prefix (cadr symbol)))
                               symbol)))
                   `(add-hook ',hook #',name)))
               (car args))
     `((define-hook!
         ,(let* ((name (or (car-safe symbol) symbol))
                 (name-args (or (cdr-safe symbol)))
                 (hook-name (or (when (eq name :anonymous) name)
                                (plist-get args :name)
                                (intern (format "%s|%s" -prefix name)))))
            (if name-args
                (cons hook-name name-args)
              hook-name))
         ,@(cdr (memq :define args)))))))

(defun core//config-advices (-prefix -advices)
  (cl-loop
   for (where symbol . args) in -advices
   for advice-name = (or (plist-get args :name)
                         (intern (format "%s*%s-%s"
                                         -prefix
                                         (substring (symbol-name where) 1)
                                         (or (plist-get args :use) symbol))))
   for form = `(advice-add ',symbol ,where #',advice-name)
   for define = (memq :define args)
   if define
   append (list `(defun ,advice-name ,@(cdr define)) form)
   else
   append (list form)))

(defmacro config! (-package-name &rest -body)
  (declare (indent 1) (debug t))
  (let* ((args (let (element result)
                 (while (setq element (pop -body))
                   (when (keywordp element)
                     (let (group)
                       (push element result)
                       (while (and (car -body)
                                   (not (keywordp (car -body))))
                         (push (pop -body) group))
                       (push (cons (pop result) (nreverse group)) result))))
                 (nreverse result)))
         (prefix (or (plist-get args :prefix) -package-name))
         before-forms
         after-forms)
    (cl-loop
     for (key . value) in args
     for (handler . handler-args) = (alist-get key core-config-keywords-alist)
     if (not (eq key :prefix))
     if (not handler)
     do (user-error "Invalid keyword %s" key)
     else
     do (let ((order (or (plist-get handler-args :order) 1))
              (form (if (eq handler t)
                        value
                      (funcall handler prefix value))))
          (push (cons order form)
                (if (> order 0) after-forms before-forms))))
    (setq after-forms
          (cl-remove nil (mapcan #'cdr (sort after-forms (lambda (x y) (< (car x) (car y)))))))
    (setq before-forms
          (cl-remove nil (mapcan #'cdr (sort before-forms (lambda (x y) (< (car x) (car y)))))))
    `(progn
       (ignore-errors
        (eval-when-compile
          (require ',-package-name nil t)))
       ,@before-forms
       (with-eval-after-load ',-package-name
         ,@after-forms))))

(config! package
  :prefix package

  :advice
  (:override package--save-selected-packages
   :define (-value)
   (when -value
     (setq package-selected-packages -value))
   (unless after-init-time
     (add-hook 'after-init-hook #'package--save-selected-packages)))

  (:after package-generate-autoloads
   :use autoclose-autoloads
   :define (-name -pkg-dir)
   "Auto close *-autoloads.el after a package installed."
   (let ((buf (find-file-existing
               (expand-file-name (concat (if (symbolp -name)
                                             (symbol-name -name)
                                           -name)
                                         "-autoloads.el")
                                 -pkg-dir))))
     (when buf (kill-buffer buf))))

  (:after package-read-all-archive-contents
   :define ()
   (dolist (pkg package-archive-contents)
     (package//set-archive pkg (gethash (car pkg)
                                        package--required-packages))))

  :config
  ;; The index of archive represents its priority
  (setq package-archives
        '(("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
          ("gnu" . "http://elpa.emacs-china.org/gnu/")
          ("melpa" . "http://elpa.emacs-china.org/melpa/")
          ("org" . "http://elpa.emacs-china.org/org/")))
  ;; Setup to select right archive
  (setq package--priority-alist (mapcar #'car package-archives))

  (unless emacs-use-gnutls-p
    (dolist (item package-archives)
      (setcdr item (concat "http://" (string-trim-left (cdr item) "https://"))))))

(if (bound-and-true-p core-dumped)
    (setq load-path core-dumped-load-path)
  ;; add load-path’s and load autoload files
  (package-initialize))

(unless (file-exists-p emacs-autoloads-file)
  (autoload 'package/generate-autoloads "autoloads/package" nil t)
  (package/generate-autoloads))

(load emacs-autoloads-file)

(provide 'core-packages)

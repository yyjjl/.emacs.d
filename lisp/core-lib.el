;;; core-lib.el --- Libraries for configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: yyj <yeyajie1229@gmail.com>

(require 'subr-x)
(require 'cl-lib)
(eval-when-compile
  (require 'seq))

(defvar ymacs--compile-config-in-progress nil)
(defvar ymacs--loaded-features ())
(defvar ymacs--required-packages () "All required packages")
(defvar ymacs--package-content-freshed-p nil)

(defvar ymacs--buffer-visible-p t
  "A flag to indicate if the buffer is not a temporary buffer")

(defmacro expand! (-name)
  `(eval-when-compile
     (if-let ((filename (or byte-compile-current-file
                            load-file-name)))
         (expand-file-name ,-name (file-name-directory filename))
       ,-name)))

(defsubst expand-cache! (-name &optional -make-p)
  (let ((val (expand-file-name -name ymacs-cache-direcotry)))
    (when (and -make-p (not (file-exists-p val)))
      (make-directory val))
    val))

(defsubst expand-etc! (-name)
  (expand-file-name -name ymacs-etc-direcotry))

(defsubst expand-bin! (-name)
  (expand-file-name -name ymacs-etc-direcotry))

(defsubst expand-tmp! (-name)
  (expand-file-name -name temporary-file-directory))

(defsubst barf-if-not-visiting-file! ()
  (unless (buffer-file-name)
    (user-error "No file is associated with current buffer")))

(defun plist-pop! (-plist -prop)
  "Delete -PROPERTIES from -PLIST."
  (let ((head -plist)
        (plist -plist)
        prev
        value)
    (catch 'done
      (while plist
        (if (not (eq -prop (car plist)))
            (progn
              (setq prev (cdr plist))
              (setq plist (cddr plist)))

          (setq value (cadr plist))
          (if (null prev)
              (setq head (cddr plist))
            (setcdr prev (cddr plist)))
          (throw 'done t))))
    (cons value head)))

(cl-defmacro executable! (-name &key -exe -docstring -full-name)
  (declare (indent 0))

  (setq -exe (or -exe (symbol-name -name)))
  (unless (vectorp -exe)
    (setq -exe (vector -exe)))

  `(defvar ,(if -full-name -name (intern (format "ymacs-%s-path" -name)))
     (eval-when-compile
       (let ((value (or ,@(seq-map (lambda (x) `(executable-find ,x)) -exe))))
         (when (null value)
           (warn "executable %s is missing" ,-exe))
         value))
     ,-docstring))

(defmacro lambda! (&rest -body)
  "A shortcut for inline interactive lambdas.

Optional argument -BODY is the lambda body."
  (declare (doc-string 1) (indent defun))
  `(lambda () (interactive) ,@-body))

(defmacro add-transient-hook! (-hook &rest -forms)
  "Attaches transient forms to a -HOOK.

Argument -HOOK can be a quoted hook or a sharp-quoted function
(which will be advised).

-FORMS will be evaluated once when that function/hook is first
invoked, then it detaches itself."
  (declare (indent 1))
  (let* ((hook (if (consp -hook) (car -hook) -hook))
         (args (and (consp -hook) (cdr -hook)))
         (append-p (plist-get args :after))
         (override-p (plist-get args :override))
         (around-p (plist-get args :around))
         (args-list (or (plist-get args :arguments) '(&rest _)))
         (local-p (plist-get args :local))
         (fn (or (plist-get args :name)
                 (cond ((functionp hook)
                        (cl-gensym "ymacs@transient-hook-"))
                       ((symbolp hook)
                        (cl-gensym "ymacs|transient-hook-"))))))
    `(progn
       (defun ,fn ,args-list
         (prog1 (progn ,@-forms)
           ,(cond ((functionp hook) `(advice-remove ',hook ',fn))
                  ((symbolp hook) `(remove-hook ',hook ',fn ,local-p)))))
       ,(cond ((functionp hook)
               `(advice-add ',hook ,(cond (override-p :override)
                                          (around-p :around)
                                          (append-p :after)
                                          (t :before))
                            ',fn))
              ((symbolp hook)
               `(add-hook ',hook ',fn ,append-p ,local-p))))))

(defmacro define-hook! (-name -hooks &rest -body)
  "Add or define a hook function.

Argument -NAME is the name of hook function. If -NAME is of the
form (name . args), then args will become the parameter list of
the lambda function to be added. Else if -NAME is :anonymous,
a lambda function will be add to -hooks

Argument -HOOKS are hook list to add fucntion to. Every hook in
-HOOKS should either be a hook-symbol or the form of (hook-symbol . args).
The args of the second form will be passed to `add-hook'

Optional argument -BODY is the function body."
  (declare (indent defun) (debug t) (doc-string 3))
  (let* ((anonymous-p (eq -name :anonymous))
         (-symbol (cond (anonymous-p (cl-gensym "g"))
                       ((consp -name) `',(car -name))
                       ((symbolp -name) `',-name)
                       (t (user-error "Invalid hook name: %s" -name))))
         (args (when (consp -name) (cdr -name)))
         (hooks (if (listp -hooks) -hooks (list -hooks)))
         (forms (cl-loop for hook in hooks
                         for hook-name = (if (consp hook) (car hook) hook)
                         for append-p = (plist-get (cdr-safe hook) :append)
                         for local-p = (plist-get (cdr-safe hook) :local)
                         collect `(add-hook ',hook-name
                                            ,-symbol ,append-p ,local-p))))
    (if anonymous-p
        `(let ((,-symbol (lambda ,args ,@-body)))
           ,@forms)
      `(progn
         (defun ,(cadr -symbol) ,args ,@-body)
         ,@forms))))

(defmacro define-key! (&rest -args)
  "Define multiple keys in one expression.

-ARGS should be the form of (:map map :prefix prefix-key key-definitions ...)"
  (declare (indent defun))
  (let ((sym (cl-gensym))
        (map 'global-map)
        (prefix "")
        forms)
    (while (keywordp (car -args))
      (pcase (pop -args)
        (:map (setq map (pop -args)))
        (:prefix (setq prefix (pop -args)))))
    (dolist (arg -args)
      (let ((key (car arg))
            (definition (cdr arg))
            properties)
        (if (eq key :has-feature)
            (progn
              (push `(eval-when-has-feature! ,(car definition)
                       (define-key! :map ,sym :prefix ,prefix
                         ,@(cdr definition)))
                    forms))
          (when (listp definition)
            (if (not (keywordp (car definition)))
                (progn
                  (setq properties (cdr definition))
                  (setq definition (car definition)))
              (setq properties definition)
              (setq definition nil)))

          (when (and (not (null definition))
                     (symbolp definition))
            (push `(declare-function ,definition "ext:unknown") forms))

          (setq definition (or (when (and (not (null definition))
                                          (symbolp definition))
                                 `#',definition)
                               (plist-get properties :map)
                               definition))

          (dolist (k (if (listp key) key (list key)))
            (push (list 'define-key sym
                        (if (stringp k)
                            (kbd (concat prefix " " k))
                          k)
                        definition)
                  forms)))))
    `(let ((,sym ,map))
       ,@(reverse forms)
       ,sym)))

(defmacro with-local-minor-mode-map! (-mode &rest -body)
  "Overrides a minor mode keybinding for the local buffer, by
creating or altering keymaps stored in buffer-local
`minor-mode-overriding-map-alist'.

-MODE is the minor mode -symbol whose map should be overrided.

-BODY is a group of sexps for defining -key bindings

Example:

    (with-local-minor-mode-map! 'abc-mode
      (define-key abc-mode-map ...)
      ...)"
  (declare (indent 1))
  `(let* ((oldmap (cdr (assoc ,-mode minor-mode-map-alist)))
          (it (or (cdr (assoc ,-mode minor-mode-overriding-map-alist))
                  (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map oldmap)
                    (push (cons ,-mode map)
                          minor-mode-overriding-map-alist)
                    map))))
     ,@-body))

(defmacro with-temp-env! (-env &rest -body)
  (declare (indent 1))
  `(let ((process-environment (append ,-env process-environment)))
     ,@-body))

(defmacro without-user-record! (&rest -body)
  `(let (ymacs--buffer-visible-p)
     ,@-body))

(defun without-user-record!! (-fn &rest -args)
  (without-user-record! (apply -fn -args)))

(defun read-file-content! (-filename)
  "Read file named -FILENAME as string."
  (without-user-record!
   (with-temp-buffer
     (insert-file-contents-literally -filename)
     (buffer-string))))

(defun open! (&rest -file-list)
  "Open All files in -FILE-LIST in external processes."
  (cond
   ((and (display-graphic-p) sys/macp)
    (mapc (lambda (path)
            (shell-command (concat "open " (shell-quote-argument (file-truename path)))))
          -file-list))
   ((and (display-graphic-p) sys/linuxp)
    (mapc (lambda (path)
            (let ((process-connection-type nil))
              (start-process "external-process" nil "xdg-open" (file-truename path))))
          -file-list))
   (t
    (user-error "Not supported yet"))))

(defun add-auto-mode! (-mode &rest -patterns)
  "Add entries to `auto-mode-alist' to use -MODE for all given
file -PATTERNS."
  (declare (indent 1))
  (dolist (pattern -patterns)
    (add-to-list 'auto-mode-alist (cons pattern -mode))))

(defun ignore-errors! (-fn &rest -args)
  "Use for advice."
  (ignore-errors (apply -fn -args)))

(defun ignore-remote! (-fn &rest -args)
  (unless (and default-directory (file-remote-p default-directory))
    (ignore-errors (apply -fn -args))))

(defsubst buffer-temporary-p ()
  "If function `buffer-file-name' return nil or a temp file or
HTML file converted from org file, it returns t."
  (let ((filename (buffer-file-name)))
    (or (not ymacs--buffer-visible-p)
        (not filename)
        (buffer-base-buffer)
        (string-match-p (concat "^" temporary-file-directory) filename))))

(defsubst buffer-enable-rich-feature-p ()
  (not (or (buffer-temporary-p)
           (file-remote-p default-directory)
           (> buffer-saved-size ymacs-large-buffer-limit))))

(defun insert-after! (-after-value -new-value -lst)
  "Find -AFTER-VALUE and Add -NEW-VALUE to -LST after it."
  (let ((pair (memq -after-value -lst)))
    (when pair
      (setcdr pair (cons -new-value (cdr pair))))))

(defun insert-before! (-before-value -new-value -lst)
  "Find -BEFORE-VALUE and Add -NEW-VALUE to -LST after it."
  (if (equal -before-value (car -lst))
      (progn
        (setcdr -lst (cons (car -lst) (cdr -lst)))
        (setcar -lst -new-value))
    (let ((x -lst))
      (while (and (listp x) (not (equal -before-value (cadr x))))
        (setq x (cdr x)))
      (setcdr x (cons -new-value (cdr x))))))

(defsubst directory-equal-p (-d1 -d2)
  (equal (file-truename (concat -d1 "/"))
         (file-truename (concat -d2 "/"))))

(defsubst file-modification-time! (file)
  (nth 5 (file-attributes file)))

(defun change-file-encodeing! (-file &optional -encoding)
  (unless -encoding
    (setq -encoding 'utf-8-unix))
  (unless (file-directory-p -file)
    (with-temp-buffer
      (insert-file-contents -file)
      (set-buffer-file-coding-system 'utf-8-unix)
      (write-file -file))))

(defun save-dir-local-variables! (&rest -variables)
  (save-window-excursion
    (dolist (var -variables)
      (add-dir-local-variable nil var (buffer-local-value var (current-buffer))))
    (save-buffer)
    (dolist (buffer (or (ignore-errors (projectile-project-buffers))
                        (list (current-buffer))))
      (with-current-buffer buffer
        (hack-dir-local-variables-non-file-buffer)))))

(defun make-process-sentinel! (-callback -error-callback &optional -sentinel)
  (lambda (-proc -msg)
    (when -sentinel
      (funcall -sentinel -proc -msg))

    (when (memq (process-status -proc) '(exit signal))
      (let ((exit-status (process-exit-status -proc)))
        (with-current-buffer (process-buffer -proc)
          (if (= exit-status 0)
              (when -callback
                (funcall -callback -msg))
            (when -error-callback
              (funcall -error-callback exit-status -msg))))))))

(cl-defun run-compilation!
    (&key -name -callback -error-callback -command -buffer-name)
  (let* ((buffer-name (cond (-buffer-name
                             (lambda (_) -buffer-name))
                            (-name
                             (lambda (_) (format "run command: %s" -name)))))
         (command-buffer (compilation-start -command t buffer-name)))
    (with-current-buffer command-buffer
      (setq ymacs-term-exit-action 'keep)
      ;; NOTE: don't use `compilation-finish-functions' or `compilation-exit-message-function'
      ;; We should make ensure the callbacks are called in the end
      (let* ((proc (get-buffer-process command-buffer))
             (sentinel (process-sentinel proc)))
        (set-process-sentinel
         proc
         (make-process-sentinel! -callback -error-callback sentinel))))))

(cl-defun run-process!
    (&key -name -program -program-args -callback -error-callback)
  (set-process-sentinel
   (apply #'start-process
          -name                         ; name
          (format " *%s*" -name)        ; buffer
          (or -program -name)           ; program
          -program-args)
   (make-process-sentinel! -callback -error-callback)))

(defun remap! (-old -new &optional -map)
  "Remap keybindings whose prefix is -OLD-KEY to -NEW-KEY in
-MAP (default `global-map')."
  (let ((map (or -map global-map))
        (-key (string-to-list (kbd -old))))
    (while (and map -key)
      (setq map (assoc (pop -key) map)))
    (when map
      (define-key -map (kbd -new) (cdr map))
      (define-key -map (kbd -old) nil))))

(defun replace! (-value -keyword -transformer-fn)
  (if (and (not (null -value)) (listp -value))
      (if (eq (car -value) -keyword)
          (funcall -transformer-fn -value)
        (cons (replace! (car -value) -keyword -transformer-fn)
              (replace! (cdr -value) -keyword -transformer-fn)))
    -value))

(defun locate-topmost! (-filename &optional -directory -topmost)
  "Find the topmost -filename"
  (let ((parent (locate-dominating-file
                 (or -directory default-directory)
                 -filename)))
    (cond
     ((equal parent "/") parent)
     (parent (locate-topmost! -filename
                              (expand-file-name ".." parent)
                              parent))
     (t -topmost))))

(defun require! (-pkg-name)
  (add-to-list 'ymacs--required-packages -pkg-name)
  (unless (package-installed-p -pkg-name)
    (unless ymacs--package-content-freshed-p
      (package-refresh-contents)
      (setq ymacs--package-content-freshed-p t))
    (message "Installing package `%s' ..." -pkg-name)
    (let ((inhibit-message t))
      (package-install -pkg-name))))

(defmacro require-packages! (&rest -pkg-list)
  (declare (indent nil))
  (let (forms compile-forms)
    (dolist (pkg -pkg-list)
      (if (atom pkg)
          (progn (push `(require! ',pkg) forms)
                 (push `(require ',pkg) compile-forms))
        (let* ((when-form (plist-get (cdr pkg) :when))
               (form `(require! ',(car pkg)))
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

(defmacro after! (-files &rest -body)
  (declare (indent 1) (debug t))
  (let ((file (or (car-safe -files) -files))
        (rest (cdr-safe -files)))
    `(progn
       (eval-when-compile
         (require ',file nil t))
       (with-eval-after-load ',file
         ,@(if rest
               `((after! ,rest ,@-body))
             -body)))))

(defmacro load-feature! (-name)
  (let* ((feature-name (symbol-name -name))
         (directory (expand-file-name feature-name ymacs-config-directory)))
    (unless (file-directory-p directory)
      (user-error "no feature %s" -name))

    (if-let ((feature-name (intern (file-name-base feature-name)))
             (forms (cl-loop
                     for name in '("package" "functions" "hooks" "config")
                     for path = (expand-file-name name directory)
                     for full-path = (concat path ".el")
                     when (file-exists-p full-path)
                     collect `(load ,path nil t))))
        `(unless (has-feature! ',feature-name)
           ,@forms
           (add-to-list 'ymacs--loaded-features (cons ',feature-name ',-name))
           (mapc #'funcall (get ',feature-name 'after-feature-functions)))
      (user-error "empty feature %s" -name))))

(defsubst has-feature! (-name)
  (assq -name ymacs--loaded-features))

(defmacro eval-when-compile-config! (&rest -body)
  `(eval-when-compile
     (when (bound-and-true-p ymacs--compile-config-in-progress)
       ,@-body)))

(defmacro eval-when-has-feature! (-name &rest -body)
  (declare (indent 1) (debug t))
  (when (has-feature! -name)
    `(progn ,@-body)))

(defmacro eval-if-has-feature! (-name -if-body &rest -else-body)
  (declare (indent 2) (debug t))
  (if (has-feature! -name)
      -if-body
    `(progn ,@-else-body)))

(defmacro after-feature! (-name &rest -body)
  (declare (indent 1) (debug t))
  `(let ((func (lambda () ,@-body)))
     (if (has-feature! ',-name)
         (funcall func)
       (put ',-name 'after-feature-functions
            (cons func (get ',-name 'after-feature-functions))))))

(defun completing-read--prompt (-prompt -return-prompt -collection)
  (let ((return-prompt (if -return-prompt
                           (format "[RET] => %s \n" -return-prompt)
                         ""))
        (keys-prompt (string-join
                      (--map-indexed
                       (format "[%d] => %s" it-index (or (car-safe it) it))
                       -collection)
                      "\n")))
    (format "%s\n%s%s" -prompt return-prompt keys-prompt)))

(cl-defun completing-read!
    (&key -prompt -collection -action -return-action -return-prompt -history)
  (if (or (not (listp -collection))
          (> (length -collection) 10))
      (funcall -action (completing-read -prompt -collection nil t nil -history))

    (let* ((max-mini-window-height 1.0)
           (key (read-key (completing-read--prompt -prompt -return-prompt -collection))))
      (cond
        ((and (>= key ?0) (< key ?9))
         (funcall -action (nth (- key ?0) -collection)))

        ((and -return-action (equal key 13))
         (funcall -return-action))

        (t (user-error "No action for key `%s'" (key-description `[,key])))))))

(cl-defun completing-read-simple!
    (&key -prompt -collection (-return-value 'unset) -return-prompt -history)
  (completing-read!
   :-prompt -prompt
   :-collection -collection
   :-action #'identity
   :-return-action (if (eq -return-value 'unset)
                       (lambda () (car -collection))
                     (lambda () -return-value))
   :-return-prompt (if (eq -return-value 'unset)
                       "[0]"
                     -return-prompt)
   :-history -history))

(defcustom ymacs-lsp-project-state :enabled
  "Whether to enable lsp in current project"
  :group 'ymacs
  :type '(choice
          (const :tag "Enable LSP in current file" :enabled)
          (const :tag "Disable LSP in current file" :disabled))
  :safe #'(lambda (x) (memq x '(:enabled :disabled))))

(cl-defmacro try-enable-lsp!
    (-name &key (-pre-init nil) (-condition t) (-init nil) (-fallback nil))
  (declare (indent 1))
  `(eval-if-has-feature! lsp
       (progn
         ,-pre-init
         (add-transient-hook!
             (hack-local-variables-hook
              :local t
              :name ,(intern (format "ymacs-lsp//%s-internal" -name)))
           (if (and ,-condition
                    (eq ymacs-lsp-project-state :enabled)
                    ,(let ((symbol (intern (format "ymacs-%s-lsp" -name))))
                       `(not (and (boundp ',symbol)
                                  (eq ,symbol :disabled))))
                    (ignore-errors (lsp))
                    (bound-and-true-p lsp-mode))
               ,-init
             ,-fallback)))
     ,-fallback))

(provide 'core-lib)

;;; core-lib.el ends here

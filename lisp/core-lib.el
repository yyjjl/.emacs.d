;;; core-lib.el --- Libraries for configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: yyj <yeyajie1229@gmail.com>

;;; Commentary:

;; This module contains some useful macros and functions
;; Naming conventions:
;;
;;   <module>-...   public variables
;;   <module>--...  private varibales
;;   <module>/...   public functions
;;   <module>//...   private functions
;;   <module>|...   hook functions
;;   <module>@...   advising functions
;;   ...!       a macro or function defined in core-lib.el

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(eval-when-compile
  (require 'seq))

(defvar ymacs--loaded-features ())

(defvar ymacs--buffer-visible-p t
  "A flag to indicate if the buffer is not a temporary buffer")

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

(defun keyword-get! (-plist -key)
  "Get values of keyword -KEY from -PLIST. This function is used
for defining functions."
  (let (forms)
    (while (and -plist (not (eq -key (pop -plist)))))
    (while (and -plist (not (keywordp (car -plist))))
      (push (pop -plist) forms))
    (nreverse forms)))

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

(cl-defmacro executable! (name &key exe doc full-name)
  (declare (indent 0))

  (setq exe (or exe (symbol-name name)))
  (unless (vectorp exe)
    (setq exe (vector exe)))

  `(defvar ,(if full-name name (intern (format "ymacs-%s-path" name)))
     (eval-when-compile
       (let ((value (or ,@(seq-map (lambda (x) `(executable-find ,x)) exe))))
         (when (null value)
           (warn "executable %s is missing" ,exe))
         value))
     ,doc))

(defmacro set-option! (-name -value)
  `(progn
     (put ',-name 'ymacs-option t)
     (put ',-name 'ymacs-option-value ,-value)))

(defmacro define-option! (-name -default-value &rest -body)
  (let (docstring)
    (when (stringp (car -body))
      (setq docstring (pop -body)))
    `(progn
       (defvar ,-name ,-default-value ,docstring)
       (when-let (-the-value (and (get ',-name 'ymacs-option)
                                  (get ',-name 'ymacs-option-value)))
         ,@-body))))

(defmacro define-variable! (&rest -body)
  (declare (indent 0))

  (let ((fmt "emacs-use-%s-p"))
    (while (keywordp (car -body))
      (pcase (pop -body)
        (:format (setq fmt (pop -body)))
        (:pkg (setq fmt (concat (symbol-name (pop -body)) "-use-%s-p")))))
    `(progn
       ,@(cl-loop
          for variable in -body
          collect
          (let* ((args (when (listp variable)
                         (prog1 (cdr variable)
                           (setq variable (car variable)))))
                 (name (or (plist-get args :name)
                           (intern (format fmt (symbol-name variable)))))
                 (value (if-let ((value (plist-get args :value)))
                            `(eval-when-compile ,value)
                          `(eval-when-compile
                             (executable-find ,(or (plist-get args :exe)
                                                   (symbol-name variable)))))))
            `(defvar ,name ,value ,(or (plist-get args :doc) "")))))))

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

(defun format-line! (-left-string -right-string)
  "Add space between -LEFT-STRING and -RIGHT-STRING to generate a
string with the witdh of current frame width."
  (concat -left-string
          (make-string (max 2 (- (frame-width)
                                 (+ (string-width -left-string)
                                    (string-width -right-string)
                                    (if window-system 0 1))))
                       ?\s)
          -right-string))

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

(defun symbol-orignal-function! (-symbol)
  "Return original function definition of -SYMBOL"
  (let ((function-def (advice--symbol-function -symbol)))
    (while (advice--p function-def)
      (setq function-def (advice--cdr function-def)))
    function-def))

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

(defun get-caller-name! ()
  "Get the current function' caller function name"
  (let* ((index 5)
         (frame (backtrace-frame index))
         (found 0))
    (while (not (equal found 2))
      (setq frame (backtrace-frame (cl-incf index)))
      (when (equal t (cl-first frame)) (cl-incf found)))
    (cl-second frame)))

(defun get-call-stack! ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

(defsubst directory-equal-p (-d1 -d2)
  (equal (file-truename (concat -d1 "/"))
         (file-truename (concat -d2 "/"))))

(defsubst file-modification-time! (file)
  (nth 5 (file-attributes file)))

(defun find-library-in-directory! (-name -dir)
  (setq -dir (expand-file-name -dir))
  (let ((files (directory-files -dir))
        (default-directory -dir)
        file
        lib-path)
    (while (and (not lib-path) files)
      (setq file (pop files))
      (unless (member file '("." ".."))
        (setq lib-path
              (if (file-directory-p file)
                  (ignore-errors (find-library-in-directory! -name file))
                (and (string= file -name)
                     (expand-file-name file -dir))))))
    lib-path))

(defun change-file-encodeing! (file &optional encoding)
  (unless encoding
    (setq encoding 'utf-8-unix))
  (unless (file-directory-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (set-buffer-file-coding-system 'utf-8-unix)
      (write-file file))))

(defun save-dir-local-variables! (&rest -variables)
  (save-window-excursion
    (dolist (var -variables)
      (add-dir-local-variable nil var (buffer-local-value var (current-buffer))))
    (save-buffer)
    (dolist (buffer (or (ignore-errors (projectile-project-buffers))
                        (list (current-buffer))))
      (with-current-buffer buffer
        (hack-dir-local-variables-non-file-buffer)))))

(cl-defun run-command! (&key name callback error-callback command directory buffer-name)
  (let* ((default-directory (or directory default-directory))
         (command-buffer (compilation-start
                          command
                          t
                          (if buffer-name
                              (lambda (_) buffer-name)
                            (lambda (_) (format "run command: %s" name))))))
    (with-current-buffer command-buffer
      (setq ymacs-term-exit-action 'keep)
      (when-let ((current-proc (get-buffer-process command-buffer))
                 (sentinel (process-sentinel (get-buffer-process command-buffer))))
        (set-process-sentinel
         current-proc
         (lambda (-proc -msg)
           (funcall sentinel -proc -msg)

           (when (memq (process-status -proc) '(exit signal))
             (let ((exit-status (process-exit-status -proc)))
               (if (= exit-status 0)
                   (and callback (funcall callback command-buffer -msg))
                 (and error-callback (funcall error-callback command-buffer -msg)))))))))))

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

(defun load-feature//option-to-form (-name -option)
  (let ((option
         (cond
          ((consp -option) -option)
          ((and (symbolp -option)
                (member (aref (symbol-name -option) 0) '(?- ?+)))
           (let ((option-name (symbol-name -option)))
             (cons (substring option-name 1)
                   (equal (aref option-name 0) ?+))))
          (t
           (error "option should start with +/- or be a cons")))))
    `(set-option! ,(intern (format "ymacs-%s-%s" -name (car option))) ,(cdr option))))

(defun load-feature//file-to-form (-path)
  (let ((full-path (concat (expand-file-name -path) ".el")))
    (when (file-exists-p full-path)
      `((load ,(expand-file-name -path) nil t)))))

(defmacro load-feature! (-name &rest -options)
  (let ((feature-name (symbol-name -name))
        directory
        forms)
    (setq directory
          (expand-file-name feature-name ymacs-config-directory))

    (unless (file-directory-p directory)
      (user-error "no feature %s" -name))

    (let ((default-directory directory))
      (setq forms
            (apply #'append
                   (mapcar #'load-feature//file-to-form
                           '("package" "functions" "hooks" "config")))))

    (unless forms
      (user-error "empty feature %s" -name))
    `(unless (memq ',-name ymacs--loaded-features)
       ,@(mapcar (lambda (x) (load-feature//option-to-form -name x)) -options)
       ,@forms
       (add-to-list 'ymacs--loaded-features ',-name)

       (dolist (func (get ',-name 'after-feature-functions))
         (funcall func)))))

(defun has-feature! (-name)
  (memq -name ymacs--loaded-features))

(defmacro eval-when-has-feature! (-name &rest -body)
  (declare (indent 1) (debug t))
  (when (memq -name ymacs--loaded-features)
    `(progn ,@-body)))

(defmacro eval-if-has-feature! (-name -if-body &rest -else-body)
  (declare (indent 2) (debug t))
  (if (memq -name ymacs--loaded-features)
      -if-body
    `(progn ,@-else-body)))

(defmacro after-feature! (-name &rest body)
  (declare (indent 1) (debug t))
  `(let ((func (lambda () ,@body)))
     (if (memq ',-name ymacs--loaded-features)
         (funcall func)
       (put ',-name 'after-feature-functions
            (cons func (get ',-name 'after-feature-functions))))))

(defun load-file! (-target-file)
  "Load -TARGET-FILE file.
If it doesn't exist, copy from the -TEMPLATE-FILE, then load it."
  (let ((target-file (expand-file-name (concat -target-file ".el")
                                       user-emacs-directory)))
    (unless (file-exists-p target-file)
      (let ((template-file
             (expand-etc!
              (format "%s-template.el"
                      (file-name-sans-extension (file-name-base target-file))))))
        (unless (file-exists-p template-file)
          (user-error "Template %s does't exist" template-file))
        (copy-file template-file target-file)))
    ;; Load private configuration
    (load (file-name-sans-extension target-file))))

(cl-defmacro try-enable-lsp!
    (name &key (condition t) (init nil) (fallback nil))
  (declare (indent 1))
  `(eval-if-has-feature! lsp
       (add-transient-hook!
           (hack-local-variables-hook
            :local t
            :name ,(intern (format "ymacs-lsp//%s-internal" name)))
         (if (and ,condition
                  ymacs-lsp-enable-in-project-p
                  ,(let ((symbol (intern (format "ymacs-%s-lsp" name))))
                     `(not (and (boundp ',symbol)
                                (eq ,symbol 'disabled))))
                  (ignore-errors (lsp))
                  (bound-and-true-p lsp-mode))
             ,init
           ,fallback))
     ,fallback))

(provide 'core-lib)

;;; core-lib.el ends here

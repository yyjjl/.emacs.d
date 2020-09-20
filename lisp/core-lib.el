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
;;   <module>*...   advising functions
;;   ...!       a macro or function defined in core-lib.el

;;; Code:

(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(defvar core--recentf-enabled-p t)
(defvar core--buffer-useful-p t
  "A flag to indicate if the buffer is not a temporary buffer")

(defun keyword-get! (-plist -key)
  "Get values of keyword -KEY from -PLIST. This function is used
for defining functions."
  (let (forms)
    (while (and -plist (not (eq -key (pop -plist)))))
    (while (and -plist (not (keywordp (car -plist))))
      (push (pop -plist) forms))
    (nreverse forms)))

(defun plist-delete! (-plist &rest -properties)
  "Delete -PROPERTIES from -PLIST."
  (let ((result -plist))
    (dolist (property -properties)
      (let ((-plist result) last-pos)
        (while -plist
          (if (not (eq property (car -plist)))
              (setq last-pos (cdr -plist)
                    -plist (cddr -plist))
            (if (null last-pos)
                (setq result (cddr -plist))
              (setcdr last-pos (cddr -plist)))
            (setq -plist nil)))))
    result))

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
         (args-list (or (plist-get args :arguments) '(&rest _)))
         (local-p (plist-get args :local))
         (fn (or (plist-get args :name)
                 (cond ((functionp hook)
                        (cl-gensym "core*transient-hook-"))
                       ((symbolp hook)
                        (cl-gensym "core|transient-hook-"))))))
    `(progn
       (defun ,fn ,args-list
         ,@-forms
         ,(cond ((functionp hook) `(advice-remove ',hook ',fn))
                ((symbolp hook) `(remove-hook ',hook ',fn ,local-p))))
       ,(cond ((functionp hook)
               `(advice-add ',hook ,(if override-p
                                        :override
                                      (if append-p :after :before))
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
      (let ((-key (car arg))
            (func (cdr arg)))
        (push (list 'define-key sym
                    (if (stringp -key)
                        (kbd (concat prefix " " -key))
                      -key)
                    (cond ((eq func nil) nil)
                          ((symbolp func) `(function ,func))
                          ((and (listp func)
                                (eq (car func) :map))
                           (cadr func))
                          (t func)))
              forms)))
    `(let ((,sym ,map))
       ,@forms
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
  `(let (core--buffer-useful-p
         core--recentf-enabled-p)
     ,@-body))

(defun without-user-record!! (-fn &rest -args)
  (without-user-record! (apply -fn -args)))

(defun read-file-content! (-filename)
  "Read file named -FILENAME as string."
  (without-user-record!
   (with-temp-buffer
     (insert-file-contents-literally -filename)
     (buffer-string))))

(defun open! (-file-list)
  "Open All files in -FILE-LIST in external processes."
  (cond
   ((eq system-type 'darwin)
    (mapc (lambda (path)
            (shell-command (concat "open " (shell-quote-argument path))))
          -file-list))
   ((eq system-type 'gnu/linux)
    (mapc (lambda (path) (let ((process-connection-type nil))
                           (start-process "" nil "xdg-open" path)))
          -file-list))))

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
    (or (not core--buffer-useful-p)
        (not filename)
        (buffer-base-buffer)
        (string-match (concat "^" temporary-file-directory)
                      filename))))

(defsubst buffer-too-large-p ()
  (let ((filename (buffer-file-name)))
    (or (and filename
             (> (or (nth 7 (file-attributes filename)) 0)
                core-large-buffer-size))
        (> (buffer-size)
           core-large-buffer-size))))

(defsubst buffer-enable-rich-feature-p ()
  (not (or (buffer-temporary-p)
           (buffer-too-large-p)
           (buffer-base-buffer)
           (file-remote-p default-directory))))

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

(defsubst expand-var! (-name &optional -make-p)
  (let ((val (expand-file-name -name emacs-var-direcotry)))
    (when (and -make-p (not (file-exists-p val)))
      (make-directory val))
    val))

(defsubst expand-etc! (-name)
  (expand-file-name -name emacs-etc-direcotry))

(defsubst expand-tmp! (-name)
  (expand-file-name -name temporary-file-directory))

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

(cl-defun run-command! (&key name callback error-callback command directory)
  (let ((default-directory (or directory default-directory))
        (command-buffer (compilation-start
                         command
                         nil
                         (lambda (_) (format "run command: %s" name)))))
    (with-current-buffer command-buffer
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

(unless (fboundp 'when-let*)
  (defalias 'when-let* 'when-let))

(provide 'core-lib)

;;; core-lib.el ends here

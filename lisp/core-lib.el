;;; core-lib.el --- Libraries for configuration  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2015-2017 Free Software Foundation, Inc.
;;
;; Author: yyj <yeyajie1229@gmail.com>
;;; Commentary:
;;
;;; Code:

(require 'subr-x)
(require 'cl-lib)

(eval-when-compile
  (require 'seq))

(defvar ymacs--compile-config-in-progress nil
  "When loading ~/.emacs.d/etc/setup.el, it is set to t.")
(defvar ymacs--loaded-features ()
  "All loaded features.")
(defvar ymacs--required-packages ()
  "All required packages.")
(defvar ymacs--package-content-freshed-p nil
  "A flag to indicate whether the package contents have been freshed.")

(defvar ymacs--buffer-visible-p t
  "A flag to indicate whether the buffer is not a temporary buffer.")

(defmacro expand! (-name)
  "Expand -NAME relative to current file while loading or byte-compiling."
  `(eval-when-compile
     (if-let ((filename (or (bound-and-true-p byte-compile-current-file)
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
  "Delete -PROP from -PLIST."
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

(defun seq-do-interactively! (-function -prompt-function -sequence)
  (let ((map (make-sparse-keymap))
        no-confirm
        done)
    (set-keymap-parent map y-or-n-p-map)
    (define-key map "a" (interactive! (setq no-confirm "y") (y-or-n-p-insert-y)))
    (define-key map "s" (interactive! (setq done t) (y-or-n-p-insert-n)))
    (setq map (make-composed-keymap map query-replace-map))

    (catch 'done
      (seq-doseq (-item -sequence)
        (let ((anwser (or no-confirm
                          (read-from-minibuffer
                           (concat (funcall -prompt-function -item) " (yes/no/all/stop) ")
                           nil
                           map))))
          (when done
            (message "Stopped")
            (throw 'done t))
          (when (equal anwser "y")
            (funcall -function -item)))))))

(cl-defmacro executable! (-name &key -exe -docstring -full-name)
  "Define a variable which points to a executable named -EXE.

Argument -EXE can also be a vector.

Argument -DOCSTRING is the docstring of the variable.

If argument -FULL-NAME is non-nil, it is used as the name of the variable,
Otherwise `(format \"ymacs-%s-path\" -NAME)' will be used."
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

(defmacro interactive! (&rest -body)
  "A shortcut for inline interactive lambdas.

Optional argument -BODY is the lambda body."
  (declare (doc-string 1) (indent defun))
  `(lambda ()
     (interactive)
     ,@-body))

(defmacro define-hook! (-name -hooks &rest -body)
  "Define and Add a hook function.

Argument -NAME is the name of the hook function, it should be one of these forms:
1. a symbol
2. (symbol . args), where args is the parameter list of newly defined function
3. :anonymous, which means to use a lambda function

Argument -HOOKS are a list of hooks where the newly defined function will be added to.
Each hook should either be a hook-symbol or the form of (hook-symbol . plist).

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
                         collect `(add-hook ',hook-name ,-symbol ,append-p ,local-p))))
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

          (if (and (cdr-safe key)
                   (not (consp (cdr key))))
              (setq key (cl-loop for i from (car key) to (cdr key)
                                 collect (number-to-string i)))
            (unless (listp key)
              (setq key (list key))))

          (dolist (k key)
            (push (list 'define-key sym
                        (if (stringp k)
                            (kbd (concat prefix " " k))
                          k)
                        definition)
                  forms)))))
    `(let ((,sym ,map))
       ,@(reverse forms)
       ,sym)))

(defmacro set-local-minor-mode-map! (-mode &rest -body)
  "Overrides a minor mode keybinding for the local buffer by creating or altering keymaps stored in
buffer-local `minor-mode-overriding-map-alist'.

Argument -MODE is the minor mode -symbol whose map should be overrided.

Argument -BODY is a group of sexps for defining -key bindings

Example:

    (with-local-minor-mode-map! 'abc-mode
      (define-key the-map ...)
      ...)"
  (declare (indent 1))
  `(let* ((old-map (cdr (assoc ,-mode minor-mode-map-alist)))
          (the-map (or (cdr (assoc ,-mode minor-mode-overriding-map-alist))
                       (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map old-map)
                         (push (cons ,-mode map)
                               minor-mode-overriding-map-alist)
                         map))))
     ,@-body))

(defmacro with-temp-env! (-env &rest -body)
  (declare (indent 1))
  `(let ((process-environment (append ,-env process-environment)))
     ,@-body))

(defmacro with-temp-advice! (-symbol -where -function &rest -body)
  (declare (indent 3))
  `(progn
     (advice-add ',-symbol ,-where
                 ,(if (symbolp -function)
                      `#',-function
                    -function)
                 '((name . ymacs-temp-advice)))
     (unwind-protect
         (progn ,@-body)
       (advice-remove ',-symbol 'ymacs-temp-advice))))

(defmacro with-temp-lv-message! (-fmt &rest -body)
  (declare (indent 1))
  `(unwind-protect
       (progn
         (lv-message ,@-fmt)
         ,@-body)
     (lv-delete-window)))

(defmacro with-transient-hook! (-hook &rest -body)
  "Add transient forms to a -HOOK.

Argument -HOOK can be one of these forms:
1. a symbol
2. (symbol . plist)

Argument -BODY will be evaluated once when that hook is first invoked, then it detaches itself."
  (declare (indent 1))
  (let* ((hook (or (car-safe -hook) -hook))
         (args (cdr-safe -hook))
         (args-list (or (plist-get args :arguments)
                        '(&rest _)))
         (local-p (plist-get args :local))
         (append-p (plist-get args :append)))
    `(let ((--ymacs-transient-buffer (current-buffer))
           (--ymacs-transient-hook))
       (setq --ymacs-transient-hook
             (lambda ,args-list
               ,(if local-p
                    `(with-current-buffer --ymacs-transient-buffer
                       (remove-hook ',hook --ymacs-transient-hook t))
                  `(remove-hook ',hook --ymacs-transient-hook))
               ,@-body))
       (add-hook ',hook --ymacs-transient-hook ,append-p ,local-p))))

(cl-defmacro with-transient-advice!
    ((-symbol -where -args . -advice-body) &rest -body)
  "Add transient -ADVICE-BODY as advice to a -SYMBOL, and then run -BODY."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (when (advice-function-member-p 'ymacs-transient-advice (symbol-function ',-symbol))
           (user-error "Nested transient advice is not supported"))
         (advice-add
          ',-symbol ,-where
          (lambda ,-args
            (advice-remove ',-symbol 'ymacs-transient-advice)
            ,@-advice-body)
          '((name . ymacs-transient-advice)))
         ,@-body)
     (advice-remove ',-symbol 'ymacs-transient-advice)))

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

(defun ignore-errors! (-fn &rest -args)
  "Use for advice."
  (ignore-errors (apply -fn -args)))

(defun ignore-remote! (-fn &rest -args)
  (unless (and default-directory (file-remote-p default-directory))
    (ignore-errors (apply -fn -args))))

(defsubst is-buffer-temporary! ()
  "If function `buffer-file-name' return nil or a temp file or
HTML file converted from org file, it returns t."
  (let ((filename (buffer-file-name)))
    (or (not ymacs--buffer-visible-p)
        (not filename)
        (buffer-base-buffer)
        (string-match-p (concat "^" temporary-file-directory) filename))))

(defsubst is-buffer-too-large ()
  (> buffer-saved-size ymacs-large-buffer-limit))

(defsubst is-buffer-suitable-for-coding! ()
  (not (or (is-buffer-temporary!)
           (file-remote-p default-directory)
           (is-buffer-too-large))))

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

(defsubst equal-directory! (-d1 -d2)
  (string= (file-name-as-directory -d1)
           (file-name-as-directory -d2)))

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

(defun make-process-sentinel! (-callback -error-callback -unwind &optional -sentinel)
  (lambda (-proc -msg)
    (when -sentinel
      (funcall -sentinel -proc -msg))

    (when (memq (process-status -proc) '(exit signal))
      (let ((exit-status (process-exit-status -proc)))
        (with-current-buffer (process-buffer -proc)
          (unwind-protect
              (if (= exit-status 0)
                  (when -callback
                    (funcall -callback -msg))
                (when -error-callback
                  (funcall -error-callback exit-status -msg)))
            (when -unwind
              (funcall -unwind))))))))

(cl-defun run-compilation!
    (&key -name -callback -error-callback -unwind -command -buffer-name (-comint t))
  (let* ((buffer-name (cond (-buffer-name
                             (lambda (_) -buffer-name))
                            (-name
                             (lambda (_) (format "run command: %s" -name)))))
         (command-buffer (compilation-start -command -comint buffer-name)))
    (with-current-buffer command-buffer
      (setq ymacs-term-exit-action 'keep)
      ;; NOTE: don't use `compilation-finish-functions' or `compilation-exit-message-function'
      ;; We should make ensure the callbacks are called in the end
      (let* ((proc (get-buffer-process command-buffer))
             (sentinel (process-sentinel proc)))
        (set-process-sentinel
         proc
         (make-process-sentinel! -callback -error-callback -unwind sentinel))))))

(cl-defun run-process!
    (&key -name -program -program-args -callback -error-callback -unwind)
  (set-process-sentinel
   (apply #'start-process
          -name                         ; name
          (format " *%s*" -name)        ; buffer
          (or -program -name)           ; program
          -program-args)
   (make-process-sentinel! -callback -error-callback -unwind)))

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
  (let* ((file (or (car-safe -files) -files))
         (rest (cdr-safe -files))
         (form `(with-eval-after-load ',file
                  ,@(if rest
                        `((after! ,rest ,@-body))
                      -body))))
    (if byte-compile-current-file
        (if (require file nil t)
            form
          `(with-no-warnings
             ,form))
      form)))

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

(defmacro eval-when! (-condition &rest -body)
  (declare (indent 1) (debug t))
  (when (eval -condition)
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

(defun completing-read!--prompt (-return-prompt -collection)
  (format "[RET] => %s\n%s"
          (or -return-prompt "[0]")
          (let ((index -1))
            (mapconcat
             (lambda (item)
               (cl-incf index)
               (format "[%d] => %s" index (or (car-safe item) item)))
             -collection
             "\n"))))

(defun completing-read!--get-value (-prompt -collection -return-prompt)
  (cl-assert (listp -collection) nil "Only list is supported")

  (if (> (length -collection) 10)
      (let ((value (completing-read -prompt -collection nil :require-match)))
        (if (consp (car -collection))   ; alist
            (assoc-string value -collection)
          value))
    (let ((prompt (completing-read!--prompt -return-prompt -collection)))
      (with-temp-lv-message! ("%s" prompt)
        (let ((key (read-key -prompt)))
          (cond
           ((and (>= key ?0) (<= key ?9)) (nth (- key ?0) -collection))

           ((equal key 13) 'return)

           (t (user-error "No action for key `%s'" (key-description `[,key])))))))))

(defun completing-read! (-prompt -collection &optional -return -return-prompt)
  (if (null -collection)
      nil
    (let ((value (completing-read!--get-value -prompt -collection -return-prompt)))
      (if (eq value 'return)
          (if -return
              (if (functionp -return)
                  (funcall -return)
                -return)
            (car -collection))
        value))))

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
         (with-transient-hook! (hack-local-variables-hook :local t)
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

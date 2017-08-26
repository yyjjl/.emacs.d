;;; core-lib.el
;;; Naming conventions:
;;
;;   <module>-...   public variables
;;   <module>--...  private varibales
;;   <module>/...   public functions
;;   <module>%...  private functions
;;   <module>|...   hook functions
;;   <module>*...   advising functions
;;   ...!       a macro or function defined in core-lib.el

(dolist (sym '(when-let if-let string-trim
                         string-join string-blank-p string-lessp
                         hash-table-keys))
  (autoload sym "subr-x" nil nil 'macro))

(defvar core-debug-mode nil)

(defmacro lambda! (&rest $body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1) (indent defun))
  `(lambda () (interactive) ,@$body))

(defvar core--transient-index 0)
(defmacro add-transient-hook! ($hook &rest $forms)
  "Attaches transient forms to a $HOOK.

HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append-p (eq (car $forms) :after))
        (fn (intern (cond ((functionp $hook)
                            (format "core*transient-hook-%s"
                                    (cl-incf core--transient-index)))
                           ((symbolp $hook)
                            (format "core|transient-hook-%s"
                                    (cl-incf core--transient-index)))))))
    `(when ,$hook
       (fset ',fn
             (lambda (&rest _)
               ,@$forms
               (cond ((functionp ,$hook) (advice-remove ,$hook #',fn))
                     ((symbolp ,$hook) (remove-hook ,$hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,$hook)
              (advice-add ,$hook ,(if append-p :after :before) #',fn))
             ((symbolp ,$hook)
              (add-hook ,$hook #',fn ,append-p))))))

(defmacro define-hook! ($name $hooks &rest $body)
  "Define a hook function.
Argument NAME is the name of hook function.  Argument HOOKS are
hooks to add fucntion to.  Optional argument BODY is the function
body."
  (declare (indent defun) (debug t) (doc-string 3))
  (let ((sym $name)
        args forms func)
    (when (consp $name)
      (setq args (cdr $name))
      (setq sym (car $name)))
    (setq func `(lambda ,args ,@$body))
    (dolist (hook $hooks)
      (let (append-p local-p extra)
        (when (consp hook)
          (setq extra (cdr hook))
          (setq hook (car hook))
          (while extra
            (pcase (pop extra)
              (:append (setq append-p t))
              (:local (setq local-p t)))))
        (push `(add-hook ',hook #',sym ,append-p ,local-p) forms)))
    `(progn
       (defalias ',sym (function ,func))
       ,@forms)))

(defvar core--settings nil)
(defmacro define-setting! ($key &rest $forms)
  "Define a setting macro. Like `defmacro', this should return a
form to be executed when called with `call-setting!'. FORMS are not
evaluated until `call-setting!' calls it."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp $key)
    (error "Not a valid property name: %s" $key))
  (let ((fn (intern (format "core%%set%s" $key))))
    `(progn
       (defun ,fn ,@$forms)
       (cl-pushnew ',(cons $key fn) core--settings :test #'eq :key #'car))))

(defmacro call-setting! ($key &rest $args)
  "Set an option defined by `define-setting!'. Skip if doesn't exist."
  (declare (indent defun))
  (unless $args
    (error "Empty set! for %s" $key))
  (let ((fn (cdr (assq $key core--settings))))
    (if fn
        (apply fn $args)
      (when core-debug-mode
        (message "No setting found for %s" $key)
        nil))))

(defmacro define-key! (&rest $args)
  "Define multiple keys in one expression"
  (declare (indent defun))
  (let ((sym (gensym))
        (map 'global-map)
        (prefix "")
        forms)
    (while (keywordp (car $args))
      (pcase (pop $args)
        (:map (setq map (pop $args)))
        (:prefix (setq prefix (pop $args)))))
    (dolist (arg $args)
      (let ((key (car arg))
            (func (cdr arg)))
        (push (list 'define-key sym
                    (if (vectorp key)
                        key
                      (kbd (concat prefix " " key)))
                    (list 'function func))
              forms)))
    `(let ((,sym ,map))
       ,@forms
       ,sym)))

(defun keyword-get! ($list $key)
  (let (forms)
    (while (and $list (not (eq $key (pop $list)))))
    (while (and $list (not (keywordp (car $list))))
      (push (pop $list) forms))
    (nreverse forms)))

(defun read-file-content! ($filename)
  "Read file named FILENAME as string."
  (let ((core-recentf-enabled? nil))
    (with-temp-buffer
      (insert-file-contents $filename)
      (buffer-string))))

(defun add-auto-mode! ($mode &rest $patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (declare (indent 1))
  (dolist (pattern $patterns)
    (add-to-list 'auto-mode-alist (cons pattern $mode))))

(defun derived-mode? ($modes &optional $buf)
  (unless $buf (setq $buf (current-buffer)))
  (when (buffer-live-p $buf)
    (with-current-buffer $buf
      (derived-mode-p $modes))))

(defun remap! ($old $new &optional $map)
  "Remap keybindings whose prefix is OLD-KEY to NEW-KEY in
MAP (default `global-map')."
  (let ((map (or $map global-map))
        (key (string-to-list (kbd $old))))
    (while (and map key)
      (setq map (assoc (pop key) map)))
    (when map
      (define-key $map (kbd $new) (cdr map))
      (define-key $map (kbd $old) nil))))

(defvar core--buffer-useful t)
(defun buffer-temporary? ()
  "If function `buffer-file-name' return nil or a temp file or
HTML file converted from org file."
  (let ((filename (buffer-file-name)))
    (or (not core--buffer-useful)
        (not filename)
        (string-match (concat "^" temporary-file-directory)
                      filename))))

(defun insert-after! ($val $ele $list)
  "Find $VAL and Add $ELE to $LIST after it."
  (let ((pair (memq $val $list)))
    (when pair
      (setcdr pair (cons $ele (cdr pair))))))

(defun pair-match? ($str)
  (let ((i 0)
        (count 0)
        char escaped-p quoted-p prime-p)
    (while (< i (length $str))
      (setq char (aref $str i))
      (if (and (not escaped-p) (eq char ?\\))
          (setq escaped-p t)
        (unless escaped-p
          (case char
            ((?\{ ?\( ?\[) (incf count))
            ((?\} ?\) ?\]) (decf count))
            (?\' (setq prime-p (not prime-p)))
            (?\" (setq quoted-p (not quoted-p))))))
      (incf i))
    (not (or quoted-p prime-p (/= count 0)))))

(defsubst expand-var! ($name &optional $make-p)
  (let ((val (expand-file-name $name emacs-var-direcotry)))
    (when (and $make-p (not (file-exists-p val)))
      (make-directory val))
    val))

(defsubst expand-etc! ($name)
  (expand-file-name $name emacs-etc-direcotry))

(defsubst directory-equal? ($d1 $d2)
  (equal (file-truename (concat $d1 "/"))
         (file-truename (concat $d2 "/"))))

(provide 'core-lib)

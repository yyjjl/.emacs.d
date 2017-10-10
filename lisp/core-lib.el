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
                    (cond ((eq func nil) nil)
                          ((symbolp func) `(function ,func))
                          (t func)))
              forms)))
    `(let ((,sym ,map))
       ,@forms
       ,sym)))

(defmacro with-local-minor-mode-map! ($mode &rest $body)
  "Overrides a minor mode keybinding for the local
   buffer, by creating or altering keymaps stored in buffer-local
   `minor-mode-overriding-map-alist'."
  (declare (indent 1))
  `(let* ((oldmap (cdr (assoc ,$mode minor-mode-map-alist)))
          (it (or (cdr (assoc ,$mode minor-mode-overriding-map-alist))
                  (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map oldmap)
                    (push (cons ,$mode map)
                          minor-mode-overriding-map-alist)
                    map))))
     ,@$body))

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

(defun open! ($file-list)
  (cond
   ((eq system-type 'darwin)
    (mapc (lambda (path)
            (shell-command (concat "open " (shell-quote-argument path))))
          $file-list))
   ((eq system-type 'gnu/linux)
    (mapc (lambda (path) (let ((process-connection-type nil))
                           (start-process "" nil "xdg-open" path)))
          $file-list))))

(defun add-auto-mode! ($mode &rest $patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (declare (indent 1))
  (dolist (pattern $patterns)
    (add-to-list 'auto-mode-alist (cons pattern $mode))))

(defun derived-mode? ($modes &optional $buf)
  (unless $buf (setq $buf (current-buffer)))
  (when (buffer-live-p (get-buffer $buf))
    (with-current-buffer $buf
      (derived-mode-p $modes))))

(defun format-line! ($left-str $right-str)
  (concat $left-str
          (make-string (max 2 (- (frame-width)
                                 (+ (string-width $left-str)
                                    (string-width $right-str)
                                    (if window-system 0 1))))
                       ?\s)
          $right-str))

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

(defsubst expand-var! ($name &optional $make-p)
  (let ((val (expand-file-name $name emacs-var-direcotry)))
    (when (and $make-p (not (file-exists-p val)))
      (make-directory val))
    val))

(defsubst expand-etc! ($name)
  (expand-file-name $name emacs-etc-direcotry))

(defsubst expand-tmp! ($name)
  (expand-file-name $name temporary-file-directory))

(defsubst directory-equal? ($d1 $d2)
  (equal (file-truename (concat $d1 "/"))
         (file-truename (concat $d2 "/"))))

(provide 'core-lib)

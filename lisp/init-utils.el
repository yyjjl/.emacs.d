(defmacro defhook (name hooks &rest body)
  "Define a hook function.
Argument NAME is the name of hook function.
Argument HOOKS are hooks to add fucntion to.
Optional argument BODY is the function body."
  (let (args)
    (when (consp name)
      (setq args (cdr name))
      (setq name (car name)))
    `(progn (defun ,name ,args ,@body)
            ,@(mapcar (lambda (hook)
                        (let (extra)
                          (when (consp hook)
                            (setq extra (cdr hook))
                            (setq hook (car hook)))
                          `(add-hook ',hook #',name ,@extra)))
                      hooks))))

(defun read-file-as-string (filename)
  "Read file named FILENAME as string."
  (let ((core|recentf-enabled-p nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun directory-equal-p (d1 d2)
  (equal (expand-file-name (concat d1 "/"))
         (expand-file-name (concat d2 "/"))))

(defun buffer-derived-mode-p (parent-mode &optional buf)
    (unless buf (setq buf (current-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((mode major-mode))
          (while (and mode (not (eq mode parent-mode)))
            (setq mode (get mode 'derived-mode-parent)))
          mode))))

(defvar core|load-user-major-mode-hook t)
(defun buffer-temporary-p ()
  "If function `buffer-file-name' return nil or a temp file or
HTML file converted from org file."
  (let ((filename (buffer-file-name)))
    (or (not core|load-user-major-mode-hook)
        (not filename)
        (string-match (concat "^" temporary-file-directory) filename))))

(cl-defun remove-keywords (rest)
  (unless (null rest)
    (if (keywordp (car rest))
        (remove-keywords (cddr rest))
      (cons (car rest) (remove-keywords (cdr rest))))))

(cl-defmacro define-keys
    (&rest keys &key (prefix "") (map 'global-map) &allow-other-keys)
  "Define multiple keys in one expression"
  (let ((sym (gensym)))
    `(let ((,sym ,map))
       ,@(mapcar (lambda (key)
                   (let ((k (car key))
                         (f (cdr key)))
                     (if (or (stringp k) (vectorp k))
                         `(define-key ,sym
                            ,(if (vectorp k) k
                               `(kbd ,(concat prefix " " k)))
                            ,(if (consp f) f `',f))
                       (let ((sym2 (gensym)))
                         `(dolist (,sym2 ,key)
                            (define-key ,sym (kbd (car ,sym2)) (cdr ,sym2)))))))
                 (remove-keywords keys))
       ,sym)))

(defun popup-to-buffer (buffer &optional no-select width-split height-split)
  (unless width-split (setq width-split 3))
  (let ((pos (if (> (frame-total-cols) (* 2 (frame-total-lines)))
                 'right
               'bottom)))
    (if (get-buffer-window buffer)
        (pop-to-buffer buffer)
      (popwin:popup-buffer
       buffer
       :noselect no-select
       :width (floor (/ (frame-total-cols) width-split))
       :height (if height-split
                   (floor (/ (frame-total-lines) height-split))
                 popwin:popup-window-height)
       :position pos))))

(defun remap-keybindings (old-key new-key &optional map)
  "Remap keybindings whose prefix is OLD-KEY to NEW-KEY in
MAP (default `global-map')."
  (let ((m (or map global-map))
        (key-seq (string-to-list (kbd old-key))))
    (while (and m key-seq)
      (setq m (assoc (car key-seq) m))
      (setq key-seq (cdr key-seq)))
    (when m
      (define-key map (kbd new-key) (cdr m))
      (define-key map (kbd old-key) nil))))

;; Find the directory containing a given library
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(defun add-to-list-after (val ele lst)
  "Find VAL and Add ELE to LST after it."
  (let ((pair (memq val lst)))
    (when pair
      (setcdr pair (cons ele (cdr pair))))))

(defun pair-match-p (str)
  (let ((i 0) (count 0) char escaped-p quoted-p prime-p)
    (while (< i (length str))
      (setq char (aref str i))
      (if (and (not escaped-p) (eq char ?\\))
          (setq escaped-p t)
        (unless escaped-p
          (case char
            ((?\{ ?\( ?\[)
             (setq count (1+ count)))
            ((?\} ?\) ?\])
             (setq count (1- count)))
            (?\' (setq prime-p (not prime-p)))
            (?\" (setq quoted-p (not quoted-p))))))
      (setq i (1+ i)))
    (not (or quoted-p prime-p (> count 0)))))

(defvar socks-server '("Default server" "127.0.0.1" 1080 5))
(defun core|toggle-socket-proxy ()
  (interactive)
  (if (eq url-gateway-method 'socks)
      (let ((method (function-get #'core|toggle-socket-proxy 'method)))
        (setq url-gateway-method (or method 'native))
        (message "Use method '%s" url-gateway-method))
    (function-put #'core|toggle-socket-proxy 'method url-gateway-method)
    (setq url-gateway-method 'socks)
    (message "Use socket proxy %s" socks-server)))

(defun core|ignore-error (fn &rest args)
  (ignore-errors (apply fn args)))

(defun derived-mode-parents (mode)
  (let (parents)
    (while (setq mode (get mode 'derived-mode-parent))
      (push mode parents))
    parents))

(provide 'init-utils)

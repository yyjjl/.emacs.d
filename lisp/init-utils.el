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
                          `(add-hook ',hook #',name ,@extra))) hooks))))

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
  `(progn
     ,@(mapcar (lambda (key)
                 (let ((k (car key))
                       (f (cdr key)))
                   `(define-key ,map
                      ,(if (vectorp k) k `(kbd ,(concat prefix " " k))) ',f)))
               (remove-keywords keys))))

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

(provide 'init-utils)

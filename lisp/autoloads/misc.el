;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun core/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun core/copy-this-file-to-new-file ()
  "Copy current file to a new file without close original file."
  (interactive)
  (let* ((this (current-buffer))
         (this-name (buffer-file-name))
         (name (completing-read "New file name: "
                                #'read-file-name-internal)))
    (if (and name this-name
             (string= name this-name)
             (not (get-buffer name)))
        (message "Copy failed !!!")
      (let ((buf (get-buffer-create name)))
        (with-current-buffer buf
          (insert-buffer-substring this)
          (write-file (expand-file-name name
                                        (file-name-directory this-name))))
        (switch-to-buffer buf)))))

;;;###autoload
(defun core/rename-this-file-and-buffer ($new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (completing-read "New file name: "
                                      #'read-file-name-internal)))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer $new-name)
        (message "A buffer named '%s' already exists!" $new-name)
      (progn
        (rename-file filename $new-name :ok-if-already-exists)
        (rename-buffer $new-name)
        (set-visited-file-name $new-name)
        (set-buffer-modified-p nil)))))

;;;###autoload
(defun core/create-scratch-buffer ()
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        (mode major-mode)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (pop-to-buffer (get-buffer-create bufname))
    (funcall mode)))

;;;###autoload
(defun core/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun core/current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (message "%s => yank ring" rlt)))

;;;###autoload
(defun core/occur-dwim ()
  (interactive)
  (if (region-active-p)
      (push (buffer-substring-no-properties
             (region-beginning)
             (region-end))
            regexp-history)
    (let ((sym (thing-at-point 'symbol)))
      (when (stringp sym)
        (push (concat "\\_<" (regexp-quote sym) "\\_>") regexp-history))))
  (call-interactively 'occur))

;;;###autoload
(defun core/eval-and-replace ($start $end)
  (interactive "r")
  (let ((value (eval
                `(let ((it (buffer-substring $start $end)))
                   ,(if (bound-and-true-p
                         mc--executing-command-for-fake-cursor)
                        (read (car read-expression-history))
                      (read--expression "Expression(it): "
                                        "(eval (read it))")))
                lexical-binding)))
    (delete-region $start $end)
    (save-excursion
      (goto-char $start)
      (prin1 value (current-buffer))
      (activate-mark 1)
      (goto-char $start))))

(declare-function browse-url-chrome "browse-url")
;;;###autoload
(defvar core-search-engine-alist
  '(("g" "google" "http://www.google.com/search?q=%s")
    ("q" "stackoverflow" "http://www.google.com/search?q=%s+site:stackoverflow.com")
    ("w" "wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
    ("d" "dictionary" "http://dictionary.reference.com/search?q=%s")
    ("cpp" "cpp" "https://www.google.com/search?q=cpp+%s")))

;;;###autoload
(defun core//read-search-engine ()
  (assoc-string
   (car (split-string-and-unquote
         (ivy-read "Engine: "
                   (mapcar (lambda (x)
                             (format "%-6s %s"
                                     (nth 0 x)
                                     (propertize (nth 2 x)
                                                 'face font-lock-comment-face)))
                           core-search-engine-alist)
                   :preselect "g"
                   :matcher (lambda (re candidates)
                              (when (and (stringp re)
                                         (not (string-prefix-p "^" re)))
                                (setq re (concat "^" re)))
                              (ivy--re-filter re candidates))
                   :require-match t)))
   core-search-engine-alist))

;;;###autoload
(defun core/search-in-chrome ($engine $keyword)
  (interactive
   (let ((engine (core//read-search-engine)))
     (list (nth 2 engine)
           (read-from-minibuffer (concat (nth 1 engine) ": ")
                                 nil nil 'core-search-history))))
  (unless (featurep 'browse-url)
    (require 'browse-url))
  (browse-url-chrome (format $engine $keyword))
  (run-hooks 'core-after-search-hook))

;;;###autoload
(defvar socks-server '("Default server" "127.0.0.1" 1080 5))
;;;###autoload
(defun core/toggle-socket-proxy ()
  (interactive)
  (if (eq url-gateway-method 'socks)
      (let ((method (function-get #'core/toggle-socket-proxy 'method)))
        (setq url-gateway-method (or method 'native))
        (message "Use method '%s" url-gateway-method))
    (function-put #'core/toggle-socket-proxy 'method url-gateway-method)
    (setq url-gateway-method 'socks)
    (message "Use socket proxy %s" socks-server)))

;;;###autoload
(defun core/copy-file-name (&optional $level $replace)
  "Copy current file name to king ring.
If ARG = 0 copy the current directory. If ARG > 0 copy the file
name without directory. If ARG < 0 copy the file name without
directory and extension."
  (interactive "p")
  (let ((path (or (buffer-file-name) default-directory)))
    (message "(-, +, 0) Copy => %s"
             (kill-new (case $level
                         (1 path)
                         (2 (abbreviate-file-name path))
                         (3 default-directory)
                         (4 (file-name-nondirectory path))
                         (5 (file-name-base path)))
                       $replace)))
  (set-transient-map
   (define-key! :map (make-sparse-keymap)
     ("=" . (lambda! (core/copy-file-name (max 1 (1- $level)) t)))
     ("-" . (lambda! (core/copy-file-name (min 5 (1+ $level)) t)))
     ("0" . (lambda! (core/copy-file-name 5 t))))))

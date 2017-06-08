;; elisp version of try...catch...finally
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun in-data-directory (name)
  (expand-file-name name emacs-data-directory))

;;--------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;--------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;--------------------------------------------------------
;; Find the directory containing a given library
;;--------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;--------------------------------------------------------
;; Delete the current file
;;--------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun copy-this-file-path ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (not name)
        (error "No file is currently being edited")
      (message "Copy => %s" name)
      (kill-new name))))

(defun copy-this-file-to-new-file ()
  (interactive)
  (let* ((this (current-buffer))
         (this-name (buffer-file-name))
         (name (read-string "New name: " (file-name-base this-name))))
    (if (and name this-name
            (string= name this-name)
            (not (get-buffer name)))
        (message "Copy failed !!")
      (let ((buf (get-buffer-create name)))
        (with-current-buffer buf
          (insert-buffer-substring this)
          (write-file (expand-file-name name (file-name-directory this-name))))
        (switch-to-buffer buf)))))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defmacro with-selected-frame (frame &rest forms)
  (let ((prev-frame (gensym))
        (new-frame (gensym)))
    `(progn
       (let* ((,new-frame (or ,frame (selected-frame)))
              (,prev-frame (selected-frame)))
         (select-frame ,new-frame)
         (unwind-protect
             (progn ,@forms)
           (select-frame ,prev-frame))))))

(defvar load-user-customized-major-mode-hook t)
(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or
HTML file converted from org file"
  (let ((filename (buffer-file-name)))
    (or (not load-user-customized-major-mode-hook)
        (not filename)
        (string-match (concat "^" temporary-file-directory) filename))))

(defun create-scratch-buffer nil
  "Create a new scratch buffer to work in.  (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun show-messages-buffer ()
  " show message buffer"
  (interactive)
  (popwin:popup-buffer (get-buffer "*Messages*")))

(defun remap-kbd (old-key new-key &optional map)
  (let ((m (or map global-map))
        (key-seq (string-to-list (kbd old-key))))
    (while (and m key-seq)
      (setq m (assoc (car key-seq) m))
      (setq key-seq (cdr key-seq)))
    (when m
      (define-key map (kbd new-key) (cdr m))
      (define-key map (kbd old-key) nil))))

(defun add-to-list-after (val ele lst)
  (let ((pair (memq val lst)))
    (when pair
      (setcdr pair (cons ele (cdr pair))))))

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

;;compute the length of the marked region
(defun region-length ()
  "Length of a region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; show ascii table
(defun ascii-table ()
  "Print the ascii table.  Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

;; grep and kill-ring
(defun grep-pattern-into-list (regexp)
  (let ((s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (if (not (member item items))
          (add-to-list 'items item)))
    items))

(defun grep-pattern-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "%s\n" i))))
    (kill-new rlt)
    (message "matched strings => kill-ring")
    rlt))

(defun grep-pattern-jsonize-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string, jsonize them, and insert into kill ring"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "\"%s\" : %s ,\n" i i))))
    (kill-new rlt)
    (message "matched strings => json => kill-ring")
    rlt))

;; unique lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (copy-yank-str rlt)
    (message "%s => clipboard & yank ring" rlt)))

(defun read-file-as-string (filename)
  (let ((recentf-can-track nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun open-externally ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (list (buffer-file-name))))
         (do-it-p (or (<= (length file-list) 5)
                       (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (cond
       ((eq system-type 'darwin)
        (mapc (lambda (path)
                (shell-command (concat "open " (shell-quote-argument path))))
              file-list))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (path) (let ((process-connection-type nil))
                           (start-process "" nil "xdg-open" path)))
              file-list))))))

(provide 'init-utils)

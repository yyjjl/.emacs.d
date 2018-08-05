;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun core/display-latex-fragment-at-point ()
  (interactive)
  (let ((latex-fragment
         (when-let ((bounds (if (region-active-p)
                                (cons (region-beginning) (region-end))
                              (when-let (element (org-element-at-point))
                                (cons (org-element-property :begin element)
                                      (org-element-property :end element))))))
           (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (buffer (get-buffer-create "*latex-preview*")))
    (shackle-display-buffer buffer nil '(:align below :size 0.4 :autoclose t))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (string-trim latex-fragment
                               (rx (* (any blank "\n,.!?;:")))
                               (rx (* (any blank "\n,.!?;:"))))
                  ""))
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (org-remove-latex-fragment-image-overlays)
      (let ((org-format-latex-options
             (plist-put (copy-sequence org-format-latex-options) :scale 2)))
        (org-toggle-latex-fragment '(16)))
      (when-let (window (get-buffer-window buffer))
        (fit-window-to-buffer window 15)
        (special-mode))
      (setq-local mode-line-format nil))
    (add-transient-hook! (window-configuration-change-hook
                          :name core|display-latex-fragment-at-point-hook)
      (when-let (window (get-buffer-window buffer))
        (delete-window window))
      (kill-buffer buffer))))

;;;###autoload
(defun core/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (run-hooks 'core-after-delete-this-file-hook)
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
  (interactive
   (list (let (confirm-nonexistent-file-or-buffer)
           (unless buffer-file-name
             (error "Current buffer is not visiting a file!"))
           (let ((new-name (completing-read "New file name: "
                                            #'read-file-name-internal)))
             (if (file-directory-p new-name)
                 (expand-file-name (file-name-nondirectory buffer-file-name)
                                   new-name)
               new-name)))))
  (let ((filename (buffer-file-name)))
    (when (or (not (file-exists-p $new-name))
              (not (equal filename $new-name))
              (yes-or-no-p (format "'%s' exists, overwrite it? " $new-name)))
      (set-visited-file-name $new-name)
      (rename-file filename $new-name)
      (let ((inhibit-message t))
        (recentf-cleanup))
      (run-hook-with-args 'core-after-rename-this-file-hook filename $new-name)
      (message "Rename to %s" $new-name))))

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
  (let* ((pos (point))
         (text (buffer-substring pos (1+ pos)))
         (faces (-flatten (list (get-char-property pos 'face)
                                (get-char-property 0 'face text)))))
    (message "%s" faces)))

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
    (message "(-, +, 0) Level %d => %s"
             $level
             (kill-new (case $level
                         (1 (buffer-name))
                         (2 path)
                         (3 (abbreviate-file-name path))
                         (4 default-directory)
                         (5 (file-name-nondirectory path))
                         (6 (file-name-base path)))
                       $replace)))
  (set-transient-map
   (define-key! :map (make-sparse-keymap)
     ("=" . (lambda! (core/copy-file-name (min 6 (1+ $level)) t)))
     ("-" . (lambda! (core/copy-file-name (max 1 (1- $level)) t)))
     ("0" . (lambda! (core/copy-file-name 1 t))))))

;;;###autoload
(defvar core--local-snippets-list nil)
;;;###autoload
(defun core/add-local-snippet ()
  (interactive)
  (let ((key (read-string "Snippet key: "))
        (template (read-string "Snippet template: "))
        (local-snippets-list (copy-alist core--local-snippets-list)))
    (-if-let (item (assoc-string key local-snippets-list))
        (when (yes-or-no-p (format "Key is used for %s, overwrite it" (cdr item)))
          (setcdr item template))
      (push (cons (substring-no-properties key) template) local-snippets-list)
      (message "Snippet %s => %s" key template))
    (setq-local core--local-snippets-list local-snippets-list)
    (save-dir-local-variables! 'core--local-snippets-list)))

;;;###autoload
(defun core//try-expand-local-snippets ()
  (when-let*
      ((bounds (bounds-of-thing-at-point 'word))
       (template (cdr-safe (assoc-string
                            (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))
                            core--local-snippets-list))))
    (yas-expand-snippet template (car bounds) (cdr bounds))))

;;;###autoload
(defun core/delete-local-snippet ()
  (interactive)
  (if (not core--local-snippets-list)
      (message "No local snippets")
    (let* ((key (completing-read "Snippet key: "
                                 core--local-snippets-list nil :require-match))
           (local-snippets-list (copy-alist core--local-snippets-list))
           (item (assoc-string key local-snippets-list)))
      (when (yes-or-no-p (format "Delete %s => %s? " (car item) (cdr item)))
        (setq-local core--local-snippets-list (delete item local-snippets-list))
        (save-dir-local-variables! 'core--local-snippets-list)))))

;;;###autoload
(defun core/delete-http-buffers ($force)
  (interactive "P")
  (let ((buffers (--filter (and (string-prefix-p " *http " (buffer-name it))
                                (not (process-live-p (get-buffer-process it))))
                           (buffer-list))))
    (when (and buffers
               (or (not $force)
                   (not (called-interactively-p 'interactive)))
               (yes-or-no-p (concat "Delete following buffers? \n"
                                    (string-join (mapcar #'buffer-name buffers) "\n"))))
      (dolist (buffer buffers)
        (kill-buffer buffer)))))

(autoload 'desktop-full-lock-name "desktop")
(autoload 'desktop-kill "desktop")

(defsubst core//desktop-files ()
  (--remove (member it '("." ".."))
            (--map (file-name-nondirectory it)
                   (--filter (file-exists-p (file-name-as-directory it))
                             (directory-files (expand-var! "desktop") :full)))))
;;;###autoload
(defun core/change-or-new-desktop ($name)
  (interactive (list (completing-read "Change to: "
                                      (list* "default" (core//desktop-files)))))
  (let ((new-dir (expand-file-name (if (equal $name "default")
                                       ""
                                     $name)
                                   (expand-var! "desktop"))))
    (if (file-exists-p new-dir)
        (if (not (and desktop-dirname
                      (equal (file-name-as-directory new-dir)
                             (file-name-as-directory desktop-dirname))
                      (file-exists-p (desktop-full-lock-name))))
            (desktop-change-dir new-dir)
          (error "Desktop file is in use !!"))
      (make-directory new-dir t)
      (desktop-kill)
      (desktop-save new-dir))
    (setq core-current-desktop-name $name)))

;;;###autoload
(defun core/delete-desktop ($name)
  (interactive (list (completing-read "Delete: "
                                      (core//desktop-files)
                                      nil
                                      :require-match)))
  (let* ((default (expand-var! "desktop"))
         (dir (expand-file-name $name default)))
    (when (and (file-exists-p dir)
               (not (equal default dir))
               (y-or-n-p (format "Delete desktop `%s'?" $name)))
      (when (equal (file-name-as-directory dir)
                   (file-name-as-directory desktop-dirname))
        (progn (desktop-change-dir default)
               (setq core-current-desktop-name "default")
               (message "Change to default desktop.")))
      (delete-directory dir t))))

(defun rainbow-delimiters--number-to-subscript ($char $n)
  (cond ((< $n 0))
        ((< $n 10)
         (list $char '(bc . tc) (+ ?₀ $n)))
        ((< $n 100)
         (list $char '(bc . tc) (+ ?₀ (/ $n 10)) '(bc . tc) (+ ?₀ (mod $n 10))))))

(defun rainbow-delimiters--add-depth-number ($loc $depth _match)
  (when rainbow-delimiters-count-mode
    (let* (($char (char-after $loc)))
      (when-let (components (rainbow-delimiters--number-to-subscript $char $depth))
        (compose-region $loc (1+ $loc) components)))))

(with-eval-after-load 'rainbow-delimiters
  (advice-add 'rainbow-delimiters--apply-color
              :after #'rainbow-delimiters--add-depth-number))

;;;###autoload
(define-minor-mode rainbow-delimiters-count-mode
  "Add count below parentheses."
  :init-value nil
  (unless (or (bound-and-true-p rainbow-delimiters-mode)
              (display-graphic-p))
    (setq rainbow-delimiters-count-mode nil)
    (error "rainbow-delimiters-mode is not enabled"))
  (if rainbow-delimiters-count-mode
      (setq line-spacing 0.1)
    (setq line-spacing (default-value 'line-spacing))))
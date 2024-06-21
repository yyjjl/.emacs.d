;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor//next-history-element (-arg)
  (interactive "p")
  (if (and (= minibuffer-history-position 0)
           (= (minibuffer-prompt-end) (point)))
      (let ((buffer (window-buffer (minibuffer-selected-window))))
        (insert (or (with-current-buffer buffer
                      (save-excursion
                        (when ymacs-editor-minibuffer-saved-point
                          (goto-char ymacs-editor-minibuffer-saved-point))
                        (thing-at-point 'symbol)))
                    "")))
    (next-history-element -arg)))


(defsubst ymacs-editor//minibuffer-completing-file-p ()
  "Return non-nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end) (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

(defun ymacs-editor//minibuffer-up-directory ()
  (when (and (ymacs-editor//minibuffer-completing-file-p)
             (eolp)
             (eq (char-before) ?/))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))
    t))

;;;###autoload
(defun ymacs-editor/minibuffer-delete-char ()
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (user-error "No in minibuffer"))

  (unless (ymacs-editor//minibuffer-up-directory)
    (call-interactively #'delete-backward-char)))

;;;###autoload
(defun ymacs-editor/minibuffer-delete-word ()
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (user-error "No in minibuffer"))

  (unless (ymacs-editor//minibuffer-up-directory)
    (call-interactively #'backward-kill-word)))

;;;###autoload
(defun ymacs-editor/consult-ripgrep-or-line (-arg)
  (interactive "p")

  (cond
   ((equal -arg 16)
    (isearch-mode t t))
   ((equal -arg 0)
    (isearch-mode nil t))
   ((or (not buffer-file-name)
        (buffer-narrowed-p)
        (let ((sys (coding-system-plist buffer-file-coding-system)))
          (not (memq (plist-get sys :category)
                     '(coding-category-undecided coding-category-utf-8))))
        (jka-compr-get-compression-info buffer-file-name)
        (< buffer-saved-size (* 256 1024)))
    (call-interactively #'consult-line))
   (t
    (let* ((file-name (or (file-remote-p buffer-file-name 'localname) buffer-file-name))
           (cmd-builder (lambda (_) (consult--ripgrep-make-builder (list file-name)))))
      (consult--grep "Ripgrep" cmd-builder nil nil)))))

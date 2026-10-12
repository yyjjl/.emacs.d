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
        (< (buffer-size) (* 256 1024)))
    (call-interactively #'consult-line))
   (t
    (call-interactively #'isearch-forward))))

;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 company-lsp)



(autoload 'lsp-ui-flycheck-enable "lsp-ui-flycheck")

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-imenu)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-highlight-symbol-at-point t)
  (setq lsp--show-doc-key "C-c C-d")
  (setq lsp-hover-text-function 'lsp--text-document-hover-string)
  (setq lsp--show-doc-extra-message
        (propertize (format " (`%s' to see more ...)" lsp--show-doc-key)
                    'face font-lock-comment-face))

  (define-key!
    ("M-s h h" . lsp-symbol-highlight)
    ("C-c r" . lsp-rename)
    ((kbd lsp--show-doc-key) . lsp/show-doc-at-point)
    ("C-c ." . lsp-info-under-point)
    ("C-c C-SPC" . lsp-execute-code-action))

  (define-hook! lsp|after-open (lsp-after-open-hook)
    (lsp-enable-imenu)
    (lsp-ui-flycheck-enable 1))

  (defun lsp-execute-code-action (action)
    "Execute code action ACTION."
    (interactive
     (let ((actions (seq-group-by #'lsp--command-get-title
                                  lsp-code-actions)))
       (cdr (assoc (completing-read "Select code action: " actions
                                    nil :require-match)
                   actions))))
    (lsp--execute-command action))

  (defvar lsp--code-action-overlay nil)
  (defun lsp//move-code-action-overlay ($beg $end &optional $buffer)
    (unless (overlayp lsp--code-action-overlay)
      (setq lsp--code-action-overlay (make-overlay $beg $end $buffer))
      (overlay-put lsp--code-action-overlay
                   'face '(:underline (:style wave :color "blue")))
      (overlay-put lsp--code-action-overlay
                   'modification-hooks
                   '((lambda (ov after-change &rest args)
                       (when after-change
                         (delete-overlay ov))))))
    (move-overlay lsp--code-action-overlay $beg $end $buffer))

  (defun lsp--make-code-action-callback (buf)
    (lambda (actions)
      (with-current-buffer buf
        (setq lsp-code-actions actions)
        (when lsp--code-action-overlay
          (delete-overlay lsp--code-action-overlay))
        (when actions
          (-when-let (bounds (bounds-of-thing-at-point 'symbol))
            (lsp//move-code-action-overlay (car bounds) (cdr bounds) buf))))))

  (defun lsp//get-hover-string (hover
                                renderers
                                &optional full-content-p)
    (when-let* ((contents (gethash "contents" hover)))
      ;; contents: MarkedString | MarkedString[] | MarkupContent
      (if (lsp--markup-content-p contents)
          (when full-content-p
            (lsp--render-markup-content hover))
        (let ((content-list (if (listp contents) contents (list contents)))
              extra-message)
          (unless full-content-p
            (let ((result (--separate (hash-table-p it) content-list)))
              (setq content-list (car result)
                    extra-message (and (cadr result)
                                       lsp--show-doc-extra-message))))
          (concat
           (mapconcat (lambda (e)
                        (let (renderer)
                          (if (hash-table-p e)
                              (if (setq renderer
                                        (cdr (assoc-string
                                              (gethash "language" e)
                                              renderers)))
                                  (when (gethash "value" e nil)
                                    (funcall renderer (gethash "value" e)))
                                (gethash "value" e))
                            e)))
                      content-list
                      (if full-content-p "\n\n" "\n"))
           extra-message)))))

  (defun lsp/show-doc-at-point ()
    (interactive)
    (lsp--cur-workspace-check)
    (let* ((client (lsp--workspace-client lsp--cur-workspace))
           (renderers (lsp--client-string-renderers client)))
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (hover (lsp--send-request
                          (lsp--make-request
                           "textDocument/hover"
                           (lsp--text-document-position-params))))
                  (contents (gethash "contents" hover))
                  (buffer (get-buffer-create "*lsp-doc*"))
                  (msg (lsp//get-hover-string hover renderers :full-content)))
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert msg)
          (help-mode))
        (pop-to-buffer buffer))))

  (defun lsp--make-hover-callback (renderers start end buffer)
    (lambda (hover)
      (with-current-buffer buffer
        (setq lsp--cur-hover-request-id nil))
      (when (and hover
                 (lsp--point-is-within-bounds-p start end)
                 (eq (current-buffer) buffer) (eldoc-display-message-p))
        (when-let* ((contents (gethash "contents" hover))
                    (msg (lsp//get-hover-string hover renderers)))
          (eldoc-message msg))))))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)

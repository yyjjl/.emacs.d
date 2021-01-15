;; -*- lexical-binding:t -*-

(declare-function lsp--render-element 'lsp-mode)

(setq lsp-keymap-prefix nil)

(after! lsp-mode
  (define-hook! ymacs-lsp//after-open (lsp-after-open-hook)
    ;; default to sort and filter by server
    (setq-local company-transformers nil))

  (cl-defmethod lsp-clients-extract-signature-on-hover
    :around (_contents _server-id)
    "Cut signature into a single line"
    (let ((content (string-trim-left (or (cl-call-next-method) ""))))
      (unless (equal (buffer-name) "*lsp-help*")
        (let* ((content-length (length content))
               (max-width (frame-width)))
          (when (>= content-length (- max-width 10))
            (let ((suffix (propertize
                           (format " ... (%s to see more)"
                                   (substitute-command-keys
                                    "\\[lsp-describe-thing-at-point]"))
                           'face 'font-lock-comment-face)))
              (setq content
                    (--> content
                      (substring it 0 (- max-width 30))
                      (replace-regexp-in-string "\n+" "｜" it t t)
                      (concat it suffix)))
              (remove-text-properties 0 (length content) '(display) content)))))
      content))

  (cl-defmethod lsp-clients-extract-signature-on-hover (-contents _server-id)
    "Extract a representative line from CONTENTS, to show in the echo area."
    (lsp--render-element -contents))

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty)
  (advice-add #'lsp-async-start-process :override #'lsp-async-start-process@pretty))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))

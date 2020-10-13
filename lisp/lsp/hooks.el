;; -*- lexical-binding:t -*-

(setq lsp-keymap-prefix nil)

(after! lsp-mode
  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    ;; default to sort and filter by server
    (setq-local company-transformers nil))

  (define-advice lsp-lv-message (:override (-message) escape)
    (if -message
        (progn
          (setq lsp--signature-last-buffer (current-buffer))
          (let ((lv-force-update t))
            (lv-message (replace-regexp-in-string "%" "%%" -message))))
      (lv-delete-window)))

  (define-advice lsp--render-on-hover-content
      (:around (-fn -contents -render-all) truncate-doc)
    (let ((content (funcall -fn -contents -render-all)))
      (unless (ymacs-popup//help-buffer-p (current-buffer))
        (setq content (string-trim-left (or content "")))
        (let* ((content-length (length content))
               (frame-width (frame-width))
               (split-pos
                (let ((start -1)
                      (i 0))
                  (while (and
                          (<= (cl-incf i) 3)
                          (< start content-length)
                          (setq start (string-match "$" content (1+ start)))))
                  start)))
          (when (or (< split-pos content-length)
                    (>= split-pos frame-width))
            (setq content
                  (concat
                   (substring content 0 (min split-pos (- frame-width 30)))
                   (propertize
                    (format " ... (%s to see more)"
                            (substitute-command-keys
                             "\\[lsp-describe-thing-at-point]"))
                    'face 'font-lock-comment-face)))))
        (setq content (s-replace "\n" " " content)))
      content))

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty)
  (advice-add #'lsp-async-start-process :override #'lsp-async-start-process@pretty))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))

;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 dap-mode
 company-lsp)



(require 'lsp)
(require 'lsp-clients)

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(define-hook! lsp|after-open (lsp-after-open-hook)
  (flycheck-mode 1)
  (lsp-ui-mode 1)

  ;; default to sort and filter by server
  (setq-local company-transformers nil)
  (setq-local company-lsp-cache-candidates nil)

  (add-to-list 'company-backends 'company-lsp)
  (add-to-list 'company-backends 'company-files))

(defun lsp*xref-make-match-item (-filename -location)
  (let* ((range (gethash "range" -location))
         (pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'highlight t line)
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make-match (or line -filename)
                     (xref-make-file-location -filename
                                              (1+ (gethash "line" pos-start))
                                              (gethash "character" pos-start))
                     (let ((length (- end start)))
                       (and (> length 0) (< length len) length)))))

(with-eval-after-load 'lsp
  (dap-mode 1)
  (dap-ui-mode 1)

  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-auto-configure nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-eldoc-hook '(lsp-hover))

  (define-key!
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c S" . lsp-describe-session)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action))

  (advice-add 'lsp--xref-make-item :override #'lsp*xref-make-match-item))

(with-eval-after-load 'lsp-ui
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-max-width 80))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)

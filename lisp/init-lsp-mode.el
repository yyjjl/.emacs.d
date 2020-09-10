;; -*- lexical-binding:t -*-

(require-packages! lsp-mode ivy)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)

(ignore-errors
  (require 'lsp)
  (require 'lsp-clients))

(cl-defmacro lsp//try-enable (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook! (hack-local-variables-hook :local t :name ,(intern (format "%s-internal" name)))
     (if (and ,enable
              lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(config! lsp
  :bind
  (:map lsp-mode-map
   ("M-s l" . lsp-lens-mode)
   ("M-s h h" . lsp-document-highlight)
   ("M-s '" . lsp-avy-lens)
   ("C-c R" . lsp-rename)
   ("C-c I" . lsp/ivy-workspace-symbol)
   ("C-c S" . lsp-describe-session)
   ("C-c B" . lsp-format-buffer)
   ("C-c C-d" . lsp-describe-thing-at-point)
   ("C-c C-SPC" . lsp-execute-code-action)
   ("C-S-SPC" . lsp-signature-activate))

  :hook
  (after-open
   :define (lsp-after-open-hook)
    ;; default to sort and filter by server
   (setq-local company-transformers nil))

  :advice
  (:override lsp-lv-message
   :define (message)
   (if message
       (progn
         (setq lsp--signature-last-buffer (current-buffer))
         (let ((lv-force-update t))
           (lv-message (replace-regexp-in-string "%" "%%" message))))
     (lv-delete-window)))

  (:around lsp--render-on-hover-content
   :define (-fn -contents -render-all)
   (let ((content (funcall -fn -contents -render-all)))
     (unless (core-popups//help-buffer-matcher (current-buffer))
       (let ((content-length (length content))
             (split-pos (string-match (rx line-end) content)))
         (when (or (< split-pos content-length)
                   (>= split-pos (frame-width)))
           (setq content
                 (concat (substring content 0 (min split-pos (- (frame-width) 30)))
                         (propertize
                          (format " ... (%s to see more)"
                                  (substitute-command-keys "\\[lsp-describe-thing-at-point]"))
                          'face 'font-lock-comment-face))))))
     content))

  :config
  (setq lsp-auto-configure t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-signature-doc-lines 2)
  (setq lsp-idle-delay 0.5)

  (setq lsp-enable-snippet t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil)

  (setq lsp-completion-enable t)

  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-symbol-highlighting-skip-current t)

  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-doc-lines 2)

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t)

  (add-to-list
   'hydra-local-toggles-heads-list
   '(lsp-mode
     "LSP"
     (("t i" lsp-toggle-trace-io
       "trace io" :toggle lsp-print-io)
      ("t h" lsp-toggle-symbol-highlight
       "highlight" :toggle lsp-enable-symbol-highlighting)
      ("t t" lsp-toggle-on-type-formatting
       "on type formating" :toggle lsp-enable-on-type-formatting)
      ("t s" lsp-toggle-signature-auto-activate
       "signature" :toggle lsp-signature-auto-activate)))))

(provide 'init-lsp-mode)

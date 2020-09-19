;;; -*- lexical-binding: t; -*-

(require-packages! doom-modeline)

(config! doom-modeline
  ;; :init
  ;; (advice-add
  ;;  'doom-modeline-def-modeline :around
  ;;  (lambda (-fn -name -lhs &optional -rhs)
  ;;    (funcall -fn -name (append -lhs -rhs))))

  :config
  (doom-modeline-def-segment
    buffer-info
    "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
    (let ((name (concat
                 ;; (doom-modeline-spc)
                 (doom-modeline--buffer-mode-icon)
                 (doom-modeline--buffer-state-icon)
                 (doom-modeline--buffer-name))))
      (if (and (listp mode-line-buffer-identification)
               (equal (car mode-line-buffer-identification) "%b"))
          (list name (cdr mode-line-buffer-identification))
        name)))

  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'auto)

  (setq doom-modeline-checker-simple-format nil)

  (setq doom-modeline-indent-info nil)
  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (setq doom-modeline-workspace-name nil)
  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-minor-modes nil)

  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)

  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host checker lsp buffer-position word-count selection-info)
    '(misc-info debug repl input-method indent-info buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(bar window-number matches buffer-info-simple checker lsp buffer-position word-count selection-info)
    '(misc-info debug input-method indent-info buffer-encoding major-mode process)))

;; Setup `mode-line-format'
(define-hook! mode-line|after-init-hook (after-init-hook)
  (winum-mode 1)
  (doom-modeline-mode 1)
  (setq-default mode-line-buffer-identification '("%b"))
  (setq-default mode-line-misc-info
                '((company-search-mode (" " company-search-lighter))
                  " " global-mode-string)))

(provide 'core-modeline)

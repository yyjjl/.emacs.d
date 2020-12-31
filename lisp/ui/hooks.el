;;; -*- lexical-binding: t; -*-

;; Setup `mode-line-format'
(define-hook! ymacs-ui|setup-modeline (after-init-hook)
  (winum-mode 1)

  (global-font-lock-mode 1)
  (global-page-break-lines-mode 1)
  (global-hl-todo-mode 1)

  (global-whitespace-mode 1)

  ;; (which-function-mode 1)
  (which-key-mode 1)

  (column-number-mode 1)
  (show-paren-mode 1)

  (setq-default mode-line-buffer-identification '("%b")))

(add-hook 'window-configuration-change-hook #'ymacs-modeline//set-selected-window)
(add-hook 'buffer-list-update-hook #'ymacs-modeline//set-selected-window)
(add-hook 'after-make-frame-functions #'ymacs-modeline//set-selected-window)
(add-hook 'delete-frame-functions #'ymacs-modeline//set-selected-window)

(add-function :after after-focus-change-function #'ymacs-modeline//focus-change)
(advice-add #'handle-switch-frame :after #'ymacs-modeline//focus-change)

(add-hook 'find-file-hook #'ymacs-modeline//update-buffer-file-name)
(add-hook 'after-save-hook #'ymacs-modeline//update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'ymacs-modeline//update-buffer-file-name)
(advice-add #'not-modified :after #'ymacs-modeline//update-buffer-file-name)
(advice-add #'rename-buffer :after #'ymacs-modeline//update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'ymacs-modeline//update-buffer-file-name)


;; update buffer encoding
(add-variable-watcher
 'buffer-file-coding-system
 (lambda (-sym _value _op -where)
   (when -where
     (with-current-buffer -where
       (ymacs-modeline//update-buffer-encoding (symbol-value -sym))))))

;; update vcs
(add-hook 'find-file-hook #'ymacs-modeline//update-vcs)
(add-hook 'after-save-hook #'ymacs-modeline//update-vcs)
(advice-add #'vc-refresh-state :after #'ymacs-modeline//update-vcs)

;; update checker
(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-error-level-compilation-level 'flycheck)
(add-hook 'flycheck-status-changed-functions #'ymacs-modeline//update-checker-state)
(add-hook 'flycheck-mode-hook #'ymacs-modeline//update-checker-state)


(ymacs-modeline-set! default main)
(ymacs-modeline-set! (comint term vterm compilation) shell)
(ymacs-modeline-set! dired project)
(ymacs-modeline-set! image media)
(ymacs-modeline-set! message message)
(ymacs-modeline-set! git-commit message)
(ymacs-modeline-set! magit vcs)
(ymacs-modeline-set! org-src org-src)
(ymacs-modeline-set! git-timemachine timemachine)

(ymacs-modeline-set! (prog text conf) header :header)

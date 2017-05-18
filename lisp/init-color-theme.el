(require 'color-theme)
(require 'color-theme-molokai)

(defun disable-themes-first (&rest args)
  ;; diable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
(advice-add 'load-theme :before #'disable-themes-first)

(color-theme-molokai)

;; make emacs transparent
(defvar fix-symbol-font-timer nil)
(when window-system
  (setq fix-symbol-font-timer
        (run-with-idle-timer
         2 1
         (lambda ()
           (set-fontset-font (frame-parameter nil 'font)
                             'symbol (font-spec :size 18 :family
                                                symbol-font-name))
           (cancel-timer fix-symbol-font-timer)))))

;; This line must be after color-theme-molokai! Don't know why.
(setq color-theme-illegal-faces
      "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock-\\)")

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)
;; NO tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(setq-default mode-line-format nil)

(provide 'init-color-theme)
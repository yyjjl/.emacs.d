;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; NO tool bar or scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
;; Do not show mode-line until setup finished
(setq-default mode-line-format nil)

(custom-set-faces
 '(default ((t (:foreground "#F8F8F2" :background "#1B1D1E"))))
 '(mode-line-inactive ((t (:foreground "#F8F8F2" :background "#2d2d2d"))))
 '(mode-line ((t (:foreground "#BCBCBC" :background "#181818")))))

(provide 'init-outlooking)

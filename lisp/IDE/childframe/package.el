;;; -*- lexical-binding: t; -*-

(option! eldoc-use-childfeame nil
  "use childframe to display eldoc"
  :type 'boolean)

(require-packages! company-posframe)

(after! posframe
  (advice-add #'keyboard-quit :before #'posframe-hide-all))

(after! company-posframe
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil)
  (setq company-posframe-quickhelp-delay nil)

  (define-key! :map company-posframe-active-map
    ("C-c C-d" . company-posframe-quickhelp-toggle)
    ("C-h" . company-posframe-quickhelp-toggle)
    ("C-v" . company-posframe-quickhelp-scroll-down)
    ("M-v" . company-posframe-quickhelp-scroll-up)))

(run-after-init! 100
  (when (display-graphic-p -frame)
    (company-posframe-mode 1)))

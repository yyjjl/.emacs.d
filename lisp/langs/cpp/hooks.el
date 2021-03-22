;; -*- lexical-binding:t -*-

(after! cc-mode
  (advice-add 'c-indent-line-or-region :around #'indent-for-tab-command@smart)

  (define-hook! ymacs-cpp//c-mode-common-setup (c-mode-common-hook)
    (ymacs-cpp//common-setup)

    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      ;; (hide-ifdef-mode 1)
      ;; Make a #define be left-aligned
      (setq c-electric-pound-behavior '(alignleft))
      (ymacs-cpp//cpp-setup))))

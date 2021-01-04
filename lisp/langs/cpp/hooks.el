;; -*- lexical-binding:t -*-

(after! cc-mode
  (define-hook! ymacs-cpp|common-setup (c-mode-common-hook)
    (ymacs-cpp//common-setup)

    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      ;; (hide-ifdef-mode 1)
      ;; Make a #define be left-aligned
      (setq c-electric-pound-behavior '(alignleft))
      (ymacs-cpp//font-lock-setup)
      (ymacs-cpp//cpp-setup)))

  (advice-add 'c-indent-line-or-region :around #'indent-for-tab-command@smart))

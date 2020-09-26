;; -*- lexical-binding:t -*-

(after! projectile
  (define-advice projectile--run-project-cmd (:around (-fn &rest -args) set-env)
    (with-temp-env! (ymacs-cpp//env)
      (apply -fn -args))))

(after! cc-mode
  (define-hook! ymacs-cpp|common-setup (c-mode-common-hook)
    (ymacs-cpp//common-setup)

    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      ;; (hide-ifdef-mode 1)
      ;; Make a #define be left-aligned
      (setq c-electric-pound-behavior '(alignleft))
      (ymacs-cpp//font-lock-setup)

      (when (buffer-enable-rich-feature-p)
        (ymacs-cpp//cpp-setup))))

  (advice-add 'c-indent-line-or-region :around #'indent-for-tab-command@smart))

;; -*- lexical-binding:t -*-

(add-hook 'ymacs-after-init-idle-hook #'desktop-save-mode)

(after! desktop
  (define-advice desktop-save (:around (-fn &rest -args) unless-loaded)
    (if (or (called-interactively-p 'interactive)
            desktop-file-modtime)
        (apply -fn -args)
      (message "Current desktop was not loaded from a file. Ignored")))

  (define-advice desktop-read (:around (-fn &rest -args) disable-semantic)
    "Temporarily disable semantic mode when load desktop"
    (let ((semantic-enable-p semantic-mode))
      (semantic-mode -1)
      (apply -fn -args)
      (when semantic-enable-p
        (semantic-mode 1)))))

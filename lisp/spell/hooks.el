;;; -*- lexical-binding: t; -*-

(after! ispell
  (define-advice ispell-word (:around (-fn &rest -args) set-extra-args)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; Use emacs original arguments
      ;; Donot use together
      (setq ispell-extra-args (ymacs-spell//detect-ispell-args))
      (apply -fn -args)
      ;; Restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t))))

(after! flyspell
  (advice-add #'flyspell-auto-correct-word :around #'ispell-word@set-extra-args))

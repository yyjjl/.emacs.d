;;; -*- lexical-binding: t; -*-

(defun ymacs-spell//detect-ispell-args (&optional -run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words"
  (when (and (bound-and-true-p ispell-program-name)
             (string-match "aspell$" ispell-program-name))
    (let ((args '("--sug-mode=ultra" "--lang=en_US")))
      (if -run-together
          (append args '("--run-together"
                         "--run-together-limit=16"
                         "--run-together-min=2"))
        args))))

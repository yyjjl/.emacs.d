;;; -*- lexical-binding: t; -*-

(dolist (hook (eval-when-compile
                `(,@(when (has-feature! 'rust)
                      '(rust-mode-hook))
                  ,@(when (has-feature! 'python)
                      '(python-mode-hook))
                  ,@(when (has-feature! 'web)
                      '(typescript-mode-hook
                        js-mode-hook))
                  ,@(when (has-feature! 'go)
                      '(go-mode-hook))
                  ,@(when (has-feature! 'cpp)
                      '(c-mode-common-hook)))))
  (add-hook hook #'ymacs-tabnine/enable))

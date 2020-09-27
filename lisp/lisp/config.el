;; -*- lexical-binding:t -*-

(after! lisp-mode
  ;; Add keyword `define-hook!'
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(define-hook!\\)\\_>[         '(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2
       (let ((type (get (intern-soft (match-string 1)) 'lisp-define-type)))
         (cond ((eq type 'var)
                font-lock-variable-name-face)
               ((eq type 'type)
                font-lock-type-face)
               (t font-lock-function-name-face)))
       nil t))
     ("\\_<@?\\(-[a-zA-Z]\\(?:\\sw\\|\\s_\\)*\\)"
      (1 'lisp-argument-face nil nil)))))

(after! help
  (define-key! :map help-mode-map ("R" . ymacs-lisp/remove-at-point)))

(after! elisp-mode
  (define-key! :map emacs-lisp-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . ymacs-lisp/describe-at-point))
  (define-key! :map lisp-interaction-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . ymacs-lisp/describe-at-point))

  (require 'semantic/bovine/el))

(after! lispy
  (define-key! :map lispy-mode-map ("M-n")))

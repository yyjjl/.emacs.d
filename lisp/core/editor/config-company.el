;;; -*- lexical-binding: t; -*-

(defsubst ymacs-editor//find-main-company-backend (-backends)
  (let ((x -backends))
    (while (and (consp x)
                (not (and (listp (car x)) (memq :with (car x)))))
      (setq x (cdr x)))
    x))

(make-variable-buffer-local 'company-backends)
(after! company
  (define-advice company-capf--candidates (:around (-fn &rest -args) set-completion-styles)
    (let ((completion-styles '(basic emacs22)))
      (ignore-errors (apply -fn -args))))

  (define-key! :map company-active-map
    ("C-d")
    ([tab] . ymacs-editor/complete-common)
    ("TAB" . ymacs-editor/complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (setq-default company-backends `(company-capf))

  (setq company-format-margin-function nil)
  ;; Company should be case sensitive
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-quick-access t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.2)
  (setq company-require-match nil)
  ;; (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)

  (add-to-list
   'mode-line-misc-info
   '(company-search-mode
     ("" (company-search-filtering "Filter" "Search") ": \"" company-search-string "\" "))))


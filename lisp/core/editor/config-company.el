;;; -*- lexical-binding: t; -*-

(defsubst ymacs-editor//find-main-company-backend (-backends)
  (let ((x -backends))
    (while (and (consp x)
                (not (and (listp (car x)) (memq :with (car x)))))
      (setq x (cdr x)))
    x))

(cl-defun ymacs-editor//add-company-backend
    (-backend &key ((:main -main-backend-p) t) ((:after -after) nil))
  ;; deep copy the backends list
  (let ((backends (mapcar (lambda (x) (if (consp x) (copy-sequence x) x))
                          company-backends)))
    (if -main-backend-p
        (when-let (parent-of-main-backend (ymacs-editor//find-main-company-backend backends))
          ;; remove -backend first
          (setq backends (delete -backend backends))
          ;; remove 'company-capf
          (setcar parent-of-main-backend
                  (delete 'company-capf (car parent-of-main-backend)))
          (if -after
              (insert-after! -after -backend (car parent-of-main-backend))
            (cl-pushnew -backend (car parent-of-main-backend))))
      (if -after
          (insert-after! -after -backend backends)
        (cl-pushnew -backend backends)))
    (setq-local company-backends backends)))

(make-variable-buffer-local 'company-backends)
(after! company
  (advice-add 'company-capf :around #'ignore-errors!)

  (define-key! :map company-active-map
    ("C-d")
    ([tab] . ymacs-editor/complete-common)
    ("TAB" . ymacs-editor/complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (setq-default company-backends
                `((company-capf
                   company-dabbrev-code company-keywords
                   ;; company-files
                   :with company-yasnippet
                   :separate)
                  ;; (company-gtags company-etags)
                  company-dabbrev))

  ;; Company should be case sensitive
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-char-regexp "[0-9a-zA-Z-_]")
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-numbers t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.3)
  (setq company-require-match nil)
  ;; (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-auto-commit nil)

  (setf (alist-get 'company-search-mode mode-line-misc-info)
        '(("" company-search-lighter " "))))


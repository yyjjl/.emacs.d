;;; -*- lexical-binding: t; -*-

(make-variable-buffer-local 'company-backends)

(defun company//complete-common ()
  (interactive)
  (unless (ignore-errors (yas-expand))
    (company-complete-common)))

(with-eval-after-load 'company
  (define-key! :map company-active-map
    ("C-d" . nil)
    ([tab] . company//complete-common)
    ("TAB" . company//complete-common)
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
                  (company-gtags company-etags)
                  company-dabbrev))

  ;; Company should be case sensitive
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-char-regexp "[0-9a-zA-Z-_]")
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-numbers t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.3)
  (setq company-clang-insert-arguments nil)
  (setq company-require-match nil)
  (setq company-etags-ignore-case t)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-auto-complete nil)
  ;; Not to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode comint-mode erc-mode
              gud-mode rcirc-mode shell-mode
              minibuffer-inactive-mode)))

(with-eval-after-load 'company-capf
  (advice-add 'company-capf :around #'ignore-errors!))

(defsubst company//find-main-backend (backends)
  (let ((x backends))
    (while (and (consp x)
                (not (and (listp (car x)) (memq :with (car x)))))
      (setq x (cdr x)))
    x))

(cl-defun company//add-backend (backend &key (main-backend? t) (after nil))
  ;; deep copy the backends list
  (let ((backends (mapcar (lambda (x) (if (consp x) (copy-sequence x) x))
                          company-backends)))
    (if main-backend?
        (when-let* ((parent-of-main-backend (company//find-main-backend backends)))
          ;; remove backend first
          (setq backends (delete backend backends))
          ;; remove 'company-capf
          (setcar parent-of-main-backend
                  (delete 'company-capf (car parent-of-main-backend)))
          (if after
              (insert-after! after backend (car parent-of-main-backend))
            (cl-pushnew backend (car parent-of-main-backend))))
      (if after
          (insert-after! after backend backends)
        (cl-pushnew backend backends)))
    (setq-local company-backends backends)))

(define-key!
  ("C-c <tab>" . company-complete)
  ("C-c TAB" . company-complete)
  ("C-c F" . company-files)

  ("C-}" . core/company-yasnippet)
  ([f6] . core/toggle-company-ispell))

(provide 'core-company)

(defhook lisp|minibuffer-setup (minibuffer-setup-hook minibuffer-exit-hook)
  (if (and (not (bound-and-true-p lispy-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   eval-expression-with-eldoc
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (lispy-mode 1)
    (lispy-mode -1)))


(defun lisp|common-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode 1)
  (unless (> (buffer-size) core|large-buffer-size)
    (prettify-symbols-mode 1))
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'xref-pop-marker-stack))

(defun lisp|racket-setup ()
  (lisp|common-setup)
  (rainbow-delimiters-mode 1)
  ;; (setq eldoc-documentation-function 'racket-eldoc-function)
  (when (buffer-temporary-p)
    (setq completion-at-point-functions nil))
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled)))

;; Hippie-expand
(defun lisp|hippie-expand-setup ()
  "Locally set `hippie-expand' completion functions for use with
Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

  (setq company-backends (remove 'company-capf company-backends))
  (add-to-list 'company-backends 'company-elisp)

  (unless (buffer-temporary-p)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t))
    (require 'semantic/bovine/el nil t)
    (auto-compile-on-save-mode)

    (lisp|hippie-expand-setup)

    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(let ((hooks '(lisp-mode-hook
               inferior-lisp-mode-hook
               lisp-interaction-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook #'lisp|common-setup)))




;; Highlight current sexp. Prevent flickery behaviour due to
;; `hl-sexp-mode' un-highlighting before each command
(with-eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(with-eval-after-load 'lisp-mode
  ;; Add keyword `defhook'
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defhook\\)\\_>[ 	'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2
       (let ((type (get (intern-soft (match-string 1)) 'lisp-define-type)))
         (cond ((eq type 'var)
                font-lock-variable-name-face)
               ((eq type 'type)
                font-lock-type-face)
               (t font-lock-function-name-face)))
       nil t)))))

(with-eval-after-load 'racket-mode
  ;; Hide error message when racket script is not loaded
  (defun lisp|racket-complete-at-point-advice (fn &rest args)
    (when (and (buffer-file-name) (racket--in-repl-or-its-file-p))
      (let ((result (apply fn args)))
        (when result
          (delete 'racket--get-type (delete :company-docsig result))))))
  (advice-add 'racket-complete-at-point
              :around #'lisp|racket-complete-at-point-advice)
  ;; Use `ivy' as default completion backends
  (defun racket--read-identifier (prompt default)
    (ivy-read prompt
              (racket--get-namespace-symbols)
              :initial-input default
              :preselect default)))

(defun lisp|transpose-sexp (n)
  (when (/= n 0)
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (if bounds
          (progn
            (let ((dist (or (and bounds (- (cdr bounds) (point))) 0)))
              (unless dist
                (forward-char 1))
              (transpose-sexps n)
              (backward-char dist)))
        (message "Not in a sexp")))))

(defun lisp|transpose-sexp-down (&optional n)
  (interactive "P")
  (unless n (setq n 1))
  (lisp|transpose-sexp n))

(defhydra hydra|sexp (:exit nil)
  "Sexp"
  ("SPC" (if (region-active-p)
             (deactivate-mark)
           (mark-sexp)) "mark")
  ("." lisp|transpose-sexp-down "->")
  ("," lisp|transpose-sexp-up "<-"))

(defun lisp|transpose-sexp-up (&optional n)
  (interactive "P")
  (unless n (setq n 1))
  (lisp|transpose-sexp (- 0 n)))

(define-keys
  ( "C->" . hydra|sexp/lisp|transpose-sexp-down)
  ( "C-<" . hydra|sexp/lisp|transpose-sexp-up))

(defun lisp|try-enable-lispy (map &optional level)
  (unless level (setq level 3))
  (define-keys :map map
    ;; navigation
    ("l" . special-lispy-right)
    ("h" . special-lispy-left)
    ("f" . special-lispy-flow)
    ("j" . special-lispy-down)
    ("k" . special-lispy-up)
    ("d" . special-lispy-different)
    ("P" . special-lispy-paste)
    ("y" . special-lispy-occur)
    ;; more transformations
    ("w" . special-lispy-move-up)
    ("s" . special-lispy-move-down)
    ;; marking
    ("a" . special-lispy-ace-symbol)
    ("H" . special-lispy-ace-symbol-replace)
    ("m" . special-lispy-mark-list)
    ;; dialect-specific
    ("A" . special-lispy-beginning-of-defun)
    ("c" . special-lispy-clone)
    ("q" . special-lispy-ace-paren)
    ("Q" . special-lispy-ace-char)
    ("v" . special-lispy-view)
    ("t" . special-lispy-teleport)
    ("n" . special-lispy-new-copy)
    ("b" . special-lispy-back)
    ;; digit argument
    (mapcar (lambda (x) `(,(format "%d" x) . special-digit-argument))
            (number-sequence 0 9)))
  (if (> level 1)
      (define-keys :map map
        ;; Paredit transformations
        ("/" . special-lispy-splice)
        ("+" . special-lispy-join)))
  (if (> level 2)
      (define-keys :map map
        ("-" . special-lispy-ace-subword)
        ("\"" . lispy-quotes)))
  map)

(defvar lisp|lispy-enable-alist '((haskell-mode . 1)
                                  (latex-mode . 2)
                                  (:all . 3)))

(defhook lisp|lispy-generic-setup (prog-mode-hook LaTeX-mode-hook)
  (let ((level (cdr-safe (or (assoc major-mode lisp|lispy-enable-alist)
                             (assoc :all lisp|lispy-enable-alist)))))
    (when level
      (use-local-map (lisp|try-enable-lispy
                      (copy-keymap (current-local-map))
                      level)))))

(provide 'init-lisp)

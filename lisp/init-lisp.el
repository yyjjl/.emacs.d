
(define-variable! :pkg lisp racket)

(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 elisp-def
 macrostep
 (racket-mode :when lisp-use-racket-p))

(defface lisp-argument-face
  `((t :underline t))
  "Face for arguments"
  :group 'lisp)

;; Align indent keywords
;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
(defun lisp*override-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let (#'(buffer-substring (point)
                                (progn (forward-sexp 1) (point)))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(define-hook! lisp|minibuffer-setup (minibuffer-setup-hook
                                     minibuffer-exit-hook)
  (if (and (not (bound-and-true-p lispy-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   core/eval-and-replace
                   eval-expression-with-eldoc
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (lispy-mode 1)
    (lispy-mode -1)))


(defun lisp|common-setup ()
  "Enable features useful in any Lisp mode."
  (setq-local hippie-expand-try-functions-list
              (append hippie-expand-try-functions-list '(try-complete-lisp-symbol)))

  (highlight-indentation-mode -1)
  (rainbow-delimiters-mode 1)
  ;; (rainbow-delimiters-count-mode 1)
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'xref-pop-marker-stack))

(defun lisp|racket-setup ()
  (lisp|common-setup)
  ;; (setq eldoc-documentation-function 'racket-eldoc-function)
  (unless (buffer-enable-rich-feature-p)
    (setq completion-at-point-functions nil))
  (flycheck-mode 1)

  (setq-local flycheck-check-syntax-automatically
              '(save mode-enabled))

  (with-local-minor-mode-map! 'lispy-mode
    (lispy-define-key it "e" #'racket-eval-sexp)
    (lispy-define-key it "i" #'racket-indent-sexp)))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)

  (setq-local lisp-indent-function #'lisp*override-indent-function)

  (when (buffer-enable-rich-feature-p)
    (auto-compile-on-save-mode 1)
    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook))
  (add-hook hook #'lisp|common-setup))

(config! lisp-mode
  :config
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

(config! racket-mode
  :advice
  ;; Use `ivy' as default completion backends
  (:override racket--read-identifier
   :define (-prompt -default)
   (ivy-read -prompt
             (racket--get-namespace-symbols)
             :initial-input -default
             :preselect -default))

  :config
  (add-to-list 'lispy-goto-symbol-alist
               '(racket-mode racket-visit-definition racket-mode)))

(config! helpful
  :bind
  (:map helpful-mode-map ("R" . lisp/remove-hook-at-point))
  (:map help-mode-map ("R" . lisp/remove-hook-at-point)))

(config! elisp-mode
  :bind
  (:map emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)
   ("C-c C-d" . helpful-at-point))
  (:map lisp-interaction-mode-map
   ("C-c e" . macrostep-expand)
   ("C-c C-d" . helpful-at-point))

  :config
  (require 'semantic/bovine/el))

(config! lispy
  :bind (:map lispy-mode-map ("M-n"))

  :advice
  (:around lispy-goto-symbol
   :define (-fn -symbol)
   (if (memq major-mode lispy-elisp-modes)
       (condition-case nil
           (elisp-def)
         (user-error (lispy-goto-symbol-elisp -symbol)))
     (funcall -fn -symbol))))

(provide 'init-lisp)

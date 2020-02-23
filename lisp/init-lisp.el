
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



;; Remove hook
;; @see https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-elisp.el
(defun lisp/remove-hook-at-point ()
  "Remove the hook at the point in the *Help* buffer."
  (interactive)
  (unless (or (eq major-mode 'help-mode)
              (eq major-mode 'helpful-mode)
              (string= (buffer-name) "*Help*"))
    (error "Only for help-mode or helpful-mode"))
  (let ((orig-point (point)))
    (save-excursion
      (when-let
          ((hook (progn (goto-char (point-min)) (symbol-at-point)))
           (func (when (and
                        (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                            (goto-char orig-point))
                        (sexp-at-point))
                   (end-of-sexp)
                   (backward-char 1)
                   (catch 'break
                     (while t
                       (condition-case _err
                           (backward-sexp)
                         (scan-error (throw 'break nil)))
                       (let ((bounds (bounds-of-thing-at-point 'sexp)))
                         (when (<= (car bounds) orig-point (cdr bounds))
                           (throw 'break (sexp-at-point)))))))))
        (when (yes-or-no-p (format "Remove %s from %s? " func hook))
          (remove-hook hook func)
          (if (eq major-mode 'helpful-mode)
              (helpful-update)
            (revert-buffer nil t)))))))

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

(defun edebug/remove-all-instrumentation ()
  "Remove all edebug instrumentation by visiting each function
definition and running `eval-defun`."
  (interactive)
  (mapatoms (lambda (symbol)
              (when-let (pos (car-safe (get symbol 'edebug)))
                (with-current-buffer (marker-buffer pos)
                  (goto-char (marker-position pos))
                  (eval-defun nil))))))

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
  (highlight-indentation-mode -1)
  (rainbow-delimiters-mode 1)
  ;; (rainbow-delimiters-count-mode 1)
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'xref-pop-marker-stack))

;; Hippie-expand
(defun lisp//hippie-expand-setup ()
  "Locally set `hippie-expand' completion functions for use with
Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol
               :append))

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

    (lisp//hippie-expand-setup)
    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook))
  (add-hook hook #'lisp|common-setup))



(with-eval-after-load 'lisp-mode
  (require 'hippie-exp)
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

(with-eval-after-load 'racket-mode
  (add-to-list 'lispy-goto-symbol-alist
               '(racket-mode racket-visit-definition racket-mode))

  (defun racket-indent-sexp ()
    (interactive)
    (unless (region-active-p)
      (lispy-mark-list 1))
    (call-interactively #'indent-region)
    (lispy-different))

  (defun racket-eval-sexp ()
    (interactive)
    (unless (region-active-p)
      (lispy-mark-list 1))
    (call-interactively #'racket-send-region))

  ;; Use `ivy' as default completion backends
  (defun racket--read-identifier (-prompt -default)
    (ivy-read -prompt
              (racket--get-namespace-symbols)
              :initial-input -default
              :preselect -default)))

(with-eval-after-load 'helpful
  (define-key! :map helpful-mode-map
    ("R" . lisp/remove-hook-at-point))
  (define-key! :map help-mode-map
    ("R" . lisp/remove-hook-at-point)))

(with-eval-after-load 'elisp-mode
  (require 'semantic/bovine/el)

  (define-key! :map emacs-lisp-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . helpful-at-point))

  (define-key! :map lisp-interaction-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . helpful-at-point)))

(with-eval-after-load 'lispy
  (define-key! :map lispy-mode-map ("M-n"))

  (defun lisp*around-goto-symbol (-fn -symbol)
    (if (memq major-mode lispy-elisp-modes)
        (condition-case nil
            (elisp-def)
          (error (lispy-goto-symbol-elisp -symbol)))
      (funcall -fn -symbol)))
  (advice-add 'lispy-goto-symbol :around #'lisp*around-goto-symbol))

(provide 'init-lisp)

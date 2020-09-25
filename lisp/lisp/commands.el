;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-lisp/racket-indent-sexp ()
  (interactive)
  (unless (region-active-p)
    (lispy-mark-list 1))
  (call-interactively #'indent-region)
  (lispy-different))

;;;###autoload
(defun ymacs-lisp/racket-eval-sexp ()
  (interactive)
  (unless (region-active-p)
    (lispy-mark-list 1))
  (call-interactively #'racket-send-region))

;;;###autoload
(defun ymacs-lisp/edebug-remove-all-instrumentation ()
  "Remove all edebug instrumentation by visiting each function
definition and running `eval-defun`."
  (interactive)
  (mapatoms
   (lambda (symbol)
     (when-let (pos (car-safe (get symbol 'edebug)))
       (with-current-buffer (marker-buffer pos)
         (goto-char (marker-position pos))
         (eval-defun nil))))))

;;;###autoload
(defun ymacs-lisp/describe-at-point ()
  (interactive)
  (-if-let (symbol (symbol-at-point))
      (cond
       ((and (boundp symbol) (fboundp symbol))
        (if (y-or-n-p
             (format "%s is a both a variable and a callable, show variable?" symbol))
            (describe-variable symbol)
          (describe-function symbol)))
       ((fboundp symbol)
        (describe-function symbol))
       ((boundp symbol)
        (describe-variable symbol))
       (t
        (user-error "Not bound: %S" symbol)))
    (user-error "There is no symbol at point.")))

;; Remove hook
;; @see https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-elisp.el
;;;###autoload
(defun ymacs-lisp/remove-hook-at-point ()
  "Remove the hook at the point in the *Help* buffer."
  (interactive)
  (unless (or (eq major-mode 'help-mode)
              (eq major-mode 'helpful-mode)
              (string= (buffer-name) "*Help*"))
    (user-error "Only for help-mode or helpful-mode"))
  (let ((orig-point (point))
        (hook (save-excursion
                (goto-char (point-min)) (symbol-at-point))))
    (if (and (get-text-property orig-point 'button)
             (save-excursion
               (beginning-of-line)
               (re-search-forward ":\\([a-z-]+\\) advice:" orig-point nil)))
        ;; Remove advice
        (let ((func (car (get-text-property orig-point 'help-args))))
          (unless func
            (user-error "Can not read advice symbol"))
          (when (yes-or-no-p (format "Remove advice %s from %s? " func hook))
            (advice-remove hook func)))
      ;; Remove hook
      (save-excursion
        (when-let
            ((func (when (and
                          (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                              (goto-char orig-point))
                          (sexp-at-point))
                     (thing-at-point--end-of-sexp)
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
            (remove-hook hook func)))))

    (if (eq major-mode 'helpful-mode)
        (helpful-update)
      (revert-buffer nil t))))

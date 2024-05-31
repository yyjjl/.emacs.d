;;; -*- lexical-binding: t; -*-

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
    (eldoc-print-current-symbol-info t)))

(defun ymacs-lisp//remove-hook-at-point (-hook)
  (let ((orig-point (point)))
    (save-excursion
      (let ((func
             (when (and
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
        (when (and func
                   (yes-or-no-p (format "Remove `%s' from `%s'? " func -hook)))
          (remove-hook -hook func))))))

(defun ymacs-lisp//remove-advice-at-point (-hook)
  (let ((func (car (get-text-property (point) 'help-args))))
    (unless func
      (user-error "Can not read advice symbol"))

    (when (yes-or-no-p (format "Remove advice %s from %s? " func -hook))
      (advice-remove -hook func))))

;; @see https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-elisp.el
;;;###autoload
(defun ymacs-lisp/remove-at-point ()
  "Remove the hook at the point in the *Help* buffer."
  (interactive)
  (unless (or (eq major-mode 'help-mode)
              (string= (buffer-name) "*Help*"))
    (user-error "Only for help-mode"))
  (let ((orig-point (point))
        (hook (save-excursion
                (goto-char (point-min)) (symbol-at-point))))
    (if (and (get-text-property orig-point 'button)
             (save-excursion
               (beginning-of-line)
               (re-search-forward ":\\([a-z-]+\\) advice:" orig-point nil)))
        (ymacs-lisp//remove-advice-at-point hook)
      (ymacs-lisp//remove-hook-at-point hook))
    (revert-buffer nil t)))

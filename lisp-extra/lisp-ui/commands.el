;; -*- lexical-binding:t -*-

(defun ymacs-tools//rainbow-delimiters--number-to-subscript (-char -n)
  (cond ((< -n 0))
        ((< -n 10)
         (list -char '(bc . tc) (+ ?₀ -n)))
        ((< -n 100)
         (list -char '(bc . tc) (+ ?₀ (/ -n 10)) '(bc . tc) (+ ?₀ (mod -n 10))))))

(defun ymacs-tools//rainbow-delimiters--add-depth-number (-loc -depth _match)
  (when ymacs-tools/rainbow-delimiters-count-mode
    (let* ((-char (char-after -loc)))
      (when-let (components (ymacs-tools//rainbow-delimiters--number-to-subscript -char -depth))
        (compose-region -loc (1+ -loc) components)))))

;;;###autoload
(define-minor-mode ymacs-tools/rainbow-delimiters-count-mode
  "Add count below parentheses."
  :init-value nil
  (unless (or (bound-and-true-p rainbow-delimiters-mode)
              (display-graphic-p))
    (setq ymacs-tools/rainbow-delimiters-count-mode nil)
    (user-error "rainbow-delimiters-mode is not enabled"))

  (if ymacs-tools/rainbow-delimiters-count-mode
      (progn
        (setq line-spacing 0.1)
        (advice-add
         'rainbow-delimiters--apply-color
         :after #'ymacs-tools//rainbow-delimiters--add-depth-number))
    (decompose-region (point-min) (point-max))
    (setq line-spacing (default-value 'line-spacing))
    (advice-remove
     'rainbow-delimiters--apply-color
     #'ymacs-tools//rainbow-delimiters--add-depth-number))
  (font-lock-flush))

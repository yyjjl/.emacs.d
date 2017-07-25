(defun eshell-ctrl-d-hack (fn &rest args)
  "When there is no input and press ctrl-d, exit eshell"
  (let ((kill-p (and
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (looking-at-p eshell-prompt-regexp))
                 (save-excursion
                   (goto-char (or eshell-last-output-end (point-max)))
                   (re-search-forward "\\s-+" (point-max) t)
                   (equal (point) (point-max))))))
    (if kill-p
        (kill-buffer)
      (apply fn args))))
(advice-add 'eshell-send-eof-to-process :around #'eshell-ctrl-d-hack)

(defun eshell/j (&optional arg)
  (let ((mm (make-hash-table :test #'equal))
        candidates)
    (dolist (dir (ring-elements eshell-last-dir-ring))
      (if (gethash dir mm)
          (puthash dir (1+ (gethash dir mm)) mm)
        (puthash dir 1 mm)))
    (setq candidates (sort (hash-table-keys mm) (lambda (a b)
                                                  (> (gethash a mm)
                                                     (gethash b mm)))))
    (if (and (integerp arg) (> arg 0))
        (eshell-lisp-command
         (mapconcat (lambda (s) (format "%4d %s" (gethash s mm) s))
                    (let ((nth (nthcdr (1- arg) candidates)))
                      (when nth (setcdr nth nil))
                      candidates)
                    "\n"))
      (ivy-read "Directory history: " candidates :initial-input
                (if (stringp arg) arg "")))))

(defhook term|eshell-setup (eshell-mode-hook)
  (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)
  (define-key eshell-mode-map (kbd "C-c i e") #'counsel-esh-history))

(with-no-warnings
  (setq eshell-last-dir-ring-size 512))

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(provide 'init-eshell)

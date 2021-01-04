;; -*- lexical-binding:t -*-

(require-packages! lispy racket-mode)

(after! racket-mode
  (define-key! :map racket-mode-map
    ([f5] . racket-run-with-debugging)
    ([f9] . racket-run)))

(add-to-list
 'ymacs-default-input-method-alist
 '(racket-mode racket-unicode-input-method racket-unicode))

(eval-when-compile-config!
 (require 'racket-mode)
 (cl-letf (((symbol-function #'y-or-n-p)
            (lambda (-prompt) (message "%s" -prompt)))
           ((symbol-function #'async-shell-command)
            (symbol-function #'shell-command)))
   (let ((code (racket-mode-start-faster)))
     (message "Exit with code: %s" code)
     (unless (eq code 0)
       (if-let (buffer (get-buffer "*Shell Command Output*"))
           (with-current-buffer buffer
             (message "%s" (buffer-string))))))))

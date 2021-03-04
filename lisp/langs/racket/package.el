;; -*- lexical-binding:t -*-

(require-packages! lispy racket-mode)

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
     (print! "  > Exit with code: %s\n" code)
     (unless (eq code 0)
       (if-let (buffer (get-buffer "*Shell Command Output*"))
           (with-current-buffer buffer
             (print! "  > %s\n" (buffer-string))))))))

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(racket-mode racket-run-with-debugging)))

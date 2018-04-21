(eval-when-compile
  (require 'hydra))

(defvar x-hydra-timer nil)
(defvar x-hydra-delay 0.25)
(defvar x-hydra-char ",")
(defvar-local x-hydra-last-buffer-undo-list nil)
(defun x-hydra-quit (&optional $no-hydra-quit)
  (when x-hydra-timer
    (cancel-timer x-hydra-timer)
    (setq x-hydra-timer nil))
  (unless $no-hydra-quit
    (hydra-keyboard-quit)))

(defun x-hydra-pre ()
  ;; Make sure timer is canceled
  (x-hydra-quit t)
  (setq x-hydra-timer (timer-create))
  (setq x-hydra-last-buffer-undo-list buffer-undo-list)
  (hydra-set-property 'x-hydra :verbosity 0)
  (unless buffer-read-only (insert x-hydra-char))
  ;; Start timer
  (timer-set-time x-hydra-timer
                  (timer-relative-time (current-time) x-hydra-delay))
  (timer-set-function x-hydra-timer 'x-hydra-quit)
  (timer-activate x-hydra-timer))

(defmacro x-hydra-lambda-body ($cmd-or-keys)
  `(progn
     (unless buffer-read-only
       (ignore-errors
         (let ((inhibit-message t))
           (undo)
           (setq buffer-undo-list x-hydra-last-buffer-undo-list))))
     (x-hydra-quit t)
     ,(if (functionp $cmd-or-keys)
          `(call-interactively #',$cmd-or-keys)
        `(setq unread-command-events
               ', (mapcar (lambda (c) (cons t c))
                          (listify-key-sequence (kbd $cmd-or-keys)))))))

(defmacro x-hydra-define ($char &rest $binds)
  (declare (indent 1))
  (let (form1 form2)
    (dolist (bind $binds)
      (let* ((key (car bind))
             (def (cadr bind))
             (fn (intern (format "x-hydra/lambda-%s-and-exit" key))))
        (push `(,key (x-hydra-lambda-body ,def)) form1)
        (push (cons key fn) form2)))
    `(progn
       (setq x-hydra-char ,$char)
       (defhydra x-hydra (:body-pre x-hydra-pre :color blue)
         ,@form1)
       (setq x-hydra/keymap
             (define-key! :map (make-sparse-keymap)
               ,@form2)))))

(defun x-hydra-invoker ()
  (interactive)
  (if (or (bound-and-true-p iedit-mode)
          (bound-and-true-p multiple-cursors-mode)
          defining-kbd-macro
          executing-kbd-macro)
      (unless buffer-read-only
        (call-interactively 'self-insert-command))
    (call-interactively 'x-hydra/body)))

(x-hydra-define ","
  ("f" find-file)
  ("i" "C-c i")
  ("r" "C-x r")
  ("v" "C-c C-v")
  ("x" "C-c C-x")
  ("4" "C-x 4")
  ("5" "C-x 5")
  ("p" "C-x p")
  ("," switch-to-buffer))

(global-set-key "," #'x-hydra-invoker)
(define-hook! x-hydra|setup-hook (c-mode-common-hook)
  (local-set-key "," #'x-hydra-invoker))

(provide 'init-hydra-extra)

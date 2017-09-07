(eval-when-compile
  (require 'hydra))

(defvar comma-hydra-timer nil)
(defvar comma-hydra-delay 0.5)
(defvar comma-hydra-char ",")
(defvar-local comma-hydra-last-buffer-undo-list nil)
(defun comma-hydra-quit (&optional $no-hydra-quit)
  (when comma-hydra-timer
    (cancel-timer comma-hydra-timer)
    (setq comma-hydra-timer nil))
  (unless $no-hydra-quit
    (hydra-keyboard-quit)))

(defun comma-hydra-pre ()
  ;; Make sure timer is canceled
  (comma-hydra-quit t)
  (setq comma-hydra-timer (timer-create))
  (setq comma-hydra-last-buffer-undo-list buffer-undo-list)
  (hydra-set-property 'comma-hydra :verbosity 0)
  (unless buffer-read-only (insert comma-hydra-char))
  ;; Start timer
  (timer-set-time comma-hydra-timer
                  (timer-relative-time (current-time) comma-hydra-delay))
  (timer-set-function comma-hydra-timer 'comma-hydra-quit)
  (timer-activate comma-hydra-timer))

(defmacro comma-hydra-lambda-body ($cmd-or-keys)
  `(progn
     (unless buffer-read-only
       (ignore-errors
         (let ((inhibit-message t))
           (undo)
           (setq buffer-undo-list comma-hydra-last-buffer-undo-list))))
     (comma-hydra-quit t)
     ,(if (functionp $cmd-or-keys)
          `(call-interactively #',$cmd-or-keys)
        `(setq unread-command-events
               ', (mapcar (lambda (c) (cons t c))
                          (listify-key-sequence (kbd $cmd-or-keys)))))))

(defmacro comma-hydra-define ($char &rest $binds)
  (declare (indent 1))
  (let (form1 form2)
    (dolist (bind $binds)
      (let* ((key (car bind))
             (def (cadr bind))
             (fn (intern (format "comma-hydra/lambda-%s-and-exit" key))))
        (push `(,key (comma-hydra-lambda-body ,def)) form1)
        (push (cons key fn) form2)))
    `(progn
       (setq comma-hydra-char ,$char)
       (defhydra comma-hydra (:body-pre comma-hydra-pre :color blue)
         ,@form1)
       (setq comma-hydra/keymap
             (define-key! :map (make-sparse-keymap)
               ,@form2)))))

(defun comma-hydra-invoker ()
  (interactive)
  (if (or (bound-and-true-p iedit-mode)
          (bound-and-true-p multiple-cursors-mode)
          defining-kbd-macro
          executing-kbd-macro)
      (unless buffer-read-only (insert comma-hydra-char))
    (call-interactively 'comma-hydra/body)))

(comma-hydra-define ","
  ("f" find-file)
  ("i" "C-c i")
  ("v" "C-c C-v")
  ("4" "C-x 4")
  ("5" "C-x 5")
  ("p" "C-c p")
  ("b" "C-c b"))

(global-set-key "," #'comma-hydra-invoker)
(define-hook! comma-hydra|setup-hook (c-mode-common-hook)
  (local-set-key "," #'comma-hydra-invoker))

(provide 'init-hydra-extra)

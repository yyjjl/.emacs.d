;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'hydra))

(defvar ymacs-x-hydra-timer nil)
(defvar ymacs-x-hydra-delay 0.25)
(defvar ymacs-x-hydra-char nil)

(defvar-local ymacs-x-hydra-last-buffer-undo-list nil)

(defun ymacs-x-hydra-quit (&optional -no-hydra-quit)
  (when ymacs-x-hydra-timer
    (cancel-timer ymacs-x-hydra-timer)
    (setq ymacs-x-hydra-timer nil))
  (unless -no-hydra-quit
    (hydra-keyboard-quit)))

(defun ymacs-x-hydra-pre ()
  ;; Make sure timer is canceled
  (ymacs-x-hydra-quit t)
  (setq ymacs-x-hydra-timer (timer-create))
  ;; Save undo list
  (setq ymacs-x-hydra-last-buffer-undo-list buffer-undo-list)
  (hydra-set-property 'ymacs-x-hydra :verbosity 0)
  (unless buffer-read-only (insert ymacs-x-hydra-char))
  ;; Start timer
  (timer-set-time ymacs-x-hydra-timer
                  (timer-relative-time (current-time) ymacs-x-hydra-delay))
  (timer-set-function ymacs-x-hydra-timer 'ymacs-x-hydra-quit)
  (timer-activate ymacs-x-hydra-timer))

(defmacro ymacs-x-hydra-lambda-body (-cmd-or-keys)
  `(progn
     (unless buffer-read-only
       (ignore-errors
         (let ((inhibit-message t))
           (undo)
           (setq buffer-undo-list ymacs-x-hydra-last-buffer-undo-list))))
     (ymacs-x-hydra-quit t)
     ,(if (functionp -cmd-or-keys)
          `(call-interactively #',-cmd-or-keys)
        `(setq unread-command-events
               ', (mapcar (lambda (c) (cons t c))
                          (listify-key-sequence (kbd -cmd-or-keys)))))))

(defmacro ymacs-x-hydra-define (-char &rest -binds)
  (declare (indent 1))
  (let (form1 form2)
    (dolist (bind -binds)
      (let* ((key (car bind))
             (def (cadr bind))
             (fn (intern (format "ymacs-x-hydra/lambda-%s-and-exit" key))))
        (push `(,key (ymacs-x-hydra-lambda-body ,def)) form1)
        (push (cons key fn) form2)))
    `(progn
       (setq ymacs-x-hydra-char ,-char)
       (defhydra ymacs-x-hydra (:body-pre ymacs-x-hydra-pre :color blue)
         ,@form1)
       (setq ymacs-x-hydra/keymap
             (define-key! :map (make-sparse-keymap)
               ,@form2)))))

(defun ymacs-x-hydra-invoker ()
  (interactive)
  (if (or (bound-and-true-p iedit-mode)
          (bound-and-true-p multiple-cursors-mode)
          defining-kbd-macro
          executing-kbd-macro)
      (unless buffer-read-only
        (call-interactively 'self-insert-command))
    (call-interactively 'ymacs-x-hydra/body)))

(defvar ymacs-x-hydra-minor-mode-map
  (define-key! :map (make-sparse-keymap)
    ("x" . ymacs-x-hydra-invoker)))

;;;###autoload
(define-minor-mode ymacs-x-hydra-minor-mode
  "use space as leader key"
  :init-value nil
  :keymap ymacs-x-hydra-minor-mode-map)

(ymacs-x-hydra-define "x"
  ("f" find-file)
  ("i" "C-c i")
  ("r" "C-x r")
  ("v" "C-c C-v")
  ("x" "C-c C-x")
  ("4" "C-x 4")
  ("5" "C-x 5")
  ("p" "C-x p")
  ("b" switch-to-buffer))

;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'hydra))

(defvar ymacs-x-delay 0.25)
(defvar ymacs-x-char "x")

(defvar ymacs-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ymacs-x-char) #'ymacs-x/invoker)
    map))

(defvar ymacs-x--commands nil)
(defvar ymacs-x--overlay nil)
(defvar ymacs-x--prefix-arg nil)

(add-variable-watcher
 'ymacs-x-char
 (lambda (-sym -new-value -op _where)
   (let ((old-value (symbol-value -sym)))
     (when (and (eq -op 'set)
                (not (equal old-value -new-value)))
       (define-key ymacs-x-keymap (kbd old-value) nil)
       (define-key ymacs-x-keymap (kbd -new-value) #'ymacs-x/invoker)))))

(defun ymacs-x//cleanup ()
  (when ymacs-x--overlay
    (delete-overlay ymacs-x--overlay)
    (setq ymacs-x--overlay nil)))

(defun ymacs-x//execute-command (-cmd)
  (when ymacs-x--prefix-arg
    (setq current-prefix-arg ymacs-x--prefix-arg)
    (setq ymacs-x--prefix-arg nil))

  (call-interactively -cmd))

(defun ymacs-x//simulate-events (-events)
  (when ymacs-x--prefix-arg
    (setq current-prefix-arg ymacs-x--prefix-arg)
    (setq ymacs-x--prefix-arg nil))

  (setq unread-command-events
        (append -events unread-command-events)))

(defun ymacs-x//fallback ()
  (when ymacs-x--prefix-arg
    (setq current-prefix-arg ymacs-x--prefix-arg)
    (setq ymacs-x--prefix-arg nil))

  (when ymacs-x--overlay
    (let* (ymacs-x-mode
           (cmd (key-binding ymacs-x-char)))
      (unless (commandp cmd t)
        (user-error "Don't know how to handle %s" ymacs-x-char))
      (let ((buffer (overlay-buffer ymacs-x--overlay))
            (last-command-event (car (listify-key-sequence ymacs-x-char)))
            (old-this-command this-command))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (goto-char (overlay-start ymacs-x--overlay))
            ;; Restore this-command, because we call `cmd' interactively in pre-command-hook
            (unwind-protect
                (call-interactively cmd)
              (setq this-command old-this-command))))))))

(defun ymacs-x//pred-command-funtion ()
  (remove-hook 'pre-command-hook #'ymacs-x//pred-command-funtion)

  (when (and ymacs-x--overlay
             (not (memq this-command ymacs-x--commands)))
    (let ((old-this-command this-command))
      (ymacs-x//fallback)))

  (ymacs-x//cleanup))

(defun ymacs-x//setup ()
  (ymacs-x//cleanup)

  (add-hook 'pre-command-hook #'ymacs-x//pred-command-funtion)

  ;; Display `ymacs-x-char' temporarily like `momentary-string-display'
  (setq ymacs-x--overlay (make-overlay (point) (point)))
  (overlay-put ymacs-x--overlay 'after-string ymacs-x-char))

(defun ymacs-x/invoker ()
  (interactive)
  (if (or (bound-and-true-p iedit-mode)
          (bound-and-true-p multiple-cursors-mode)
          defining-kbd-macro
          executing-kbd-macro)
      (ymacs-x//try-command)
    (ymacs-x//setup)
    (ymacs-x/body)))

;;;###autoload
(define-minor-mode ymacs-x-mode
  "use X as leader key"
  :group 'ymacs
  :init-value nil
  (ymacs-x//cleanup)
  (if ymacs-x-mode
      (setf (alist-get 'ymacs-x-mode minor-mode-overriding-map-alist) ymacs-x-keymap)
    (setf (alist-get 'ymacs-x-mode minor-mode-overriding-map-alist nil 'remove) nil)))

(defun ymacs-x-mode-turn-on ()
  (ymacs-x-mode 1))

;;;###autoload
(define-globalized-minor-mode ymacs-x-global-mode ymacs-x-mode ymacs-x-mode-turn-on)

(defmacro ymacs-x//define (&rest -binds)
  (declare (indent 0))
  `(progn
     (hydra-set-property 'ymacs-x :verbosity 0)
     (defhydra ymacs-x
       (:color blue :timeout ,ymacs-x-delay
               ;; :pre (setq hydra-hint-display-type 'message)
               ;; :post (setq hydra-hint-display-type 'lv)
               )
       ,@(cl-loop
          for (key def) in -binds
          for docstring = (if (stringp def) def (symbol-name def))
          for event = (when (stringp def)
                        (mapcar (lambda (c) (cons t c)) (listify-key-sequence (kbd def))))
          if event
          collect `(,key (ymacs-x//simulate-events ',event) ,docstring)
          else
          collect `(,key (ymacs-x//execute-command ',def) ,docstring)))

     (let ((map (make-sparse-keymap)))
       ,@(cl-loop
          for (key _) in -binds
          collect
          `(progn
             (let* ((key (kbd ,key))
                    (command (lookup-key ymacs-x/keymap key)))
               (push command ymacs-x--commands)
               (define-key map key command))))
       (setq ymacs-x/keymap map))))

(ymacs-x//define
  ("f" find-file)
  ("i" "C-c i")
  ("r" "C-x r")
  ("v" "C-c C-v")
  ("x" "C-c C-x")
  ("4" "C-x 4")
  ("5" "C-x 5")
  ("p" "C-x p")
  ("b" switch-to-buffer))

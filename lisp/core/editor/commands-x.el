;; -*- lexical-binding: t; -*-

(defvar-local ymacs-x--keymap-alist nil)

(defvar-local ymacs-x--command nil)
(defvar-local ymacs-x--activated nil)

(defvar ymacs-x-dynamic-keys
  '(("'" keyboard-quit)
    ("+" "C-+")
    ("," "C-x ,")
    ("-" "C--")
    ("." ymacs-editor/goto-last-change)
    ("/" "C-/" :exit no)
    ("?" which-key-show-top-level)
    (";" ymacs-x/just-x :exit immediate)
    ("=" "C-=")
    ("RET" "RET")
    ("SPC" "C-SPC" :exit no)
    ("TAB" "C-x b")
    ("<tab>" "C-x b")
    ("[" "C-x <left>")
    ("\\" "C-c C-b")
    ("]" "C-x <right>")
    ("`" pop-to-mark-command :exit no)
    ("a" "C-a" :exit no)
    ("b" "C-b" :exit no)
    ("c" "C-c")
    ("d" "C-c C-d")
    ("e" "C-e" :exit no)
    ("f" "C-f" :exit no)
    ("g" "M-g")
    ("h" "C-h")
    ("i" "C-c i")
    ("j" "C-c i j")
    ("k" "C-x k")
    ("l" "C-l")
    ("m" "C-c C-SPC")
    ("n" "C-n" :exit no)
    ("o" "C-x C-f")
    ("p" "C-p" :exit no)
    ("q" ymacs-x/deactivate)
    ("r" "C-c i a")
    ("s" "C-s")
    ("t" "C-c t")
    ("u" "C-u")
    ("v" "C-v" :exit no)
    ("w" ymacs-x/kill-or-save-buffer)
    ("x" "C-x")
    ("y" "C-y")
    ("z" "C-z")))

(put 'digit-argument 'ymacs-x-exit 'no)
(put 'universal-argument 'ymacs-x-exit 'no)
(put 'universal-argument-more 'ymacs-x-exit 'no)

(defvar ymacs-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ";") #'ymacs-x/activate)
    map))

(defun ymacs-x//set-ivy-minibuffer-map ()
  (define-key! :map ivy-minibuffer-map
    (("; [" "; ]" "; ;") . self-insert-command)
    ("; '" . minibuffer-keyboard-quit)
    ("; o" . ivy-occur)
    ("; RET" . ivy-immediate-done)
    ("[" . ivy-previous-line)
    ("]" . ivy-next-line)))

(defun ymacs-x//unset-ivy-minibuffer-map ()
  (define-key! :map ivy-minibuffer-map
    (( ";" "[" "]") . self-insert-command)))

(defsubst ymacs-x//lookup-keys (-keys)
  (let ((ymacs-x-mode)
        (ymacs-x--activated)
        (ymacs-x--keymap-alist))
    (key-binding -keys)))

(defun ymacs-x//create-dynamic-keymap (-dynamic-keys)
  (let (ymacs-x-mode
        ymacs-x--activated
        (map (make-sparse-keymap)))
    (suppress-keymap map)
    (cl-loop
     for i from 0 to 26
     when (or (not (= i 7)) (not (= i 8)))
     do (define-key map (char-to-string i) 'undefined))

    (cl-loop
     for (new-key old-key-or-def . props) in -dynamic-keys
     for binding = nil
     do (progn
          (if (stringp old-key-or-def)
              (progn
                (setq old-key-or-def (kbd old-key-or-def))
                (setq binding (ymacs-x//lookup-keys old-key-or-def)))
            (setq binding old-key-or-def))
          (when binding
            (when (symbolp binding)
              (let ((exit (plist-get props :exit)))
                (when exit
                  (put binding 'ymacs-x-exit exit))))
            (when (stringp old-key-or-def)
              (ignore-errors (define-key map old-key-or-def 'undefined)))
            (define-key map (kbd new-key) binding))))
    map))

(defun ymacs-x//pre-command-hook ()
  (let ((exit (get this-command 'ymacs-x-exit))
        (after-activated (eq real-last-command 'ymacs-x/activate)))
    (if (and (not after-activated)
             (eq exit 'immediate))
        (setq this-command #'ymacs-x/warn)
      (when (or (eq exit t)
                (and after-activated
                     (not (eq exit 'no))))
        (ymacs-x/deactivate)))))

(defun ymacs-x/activate ()
  (interactive)

  (unless ymacs-x--activated
    (unless (cdar ymacs-x--keymap-alist)
      (setf (cdar ymacs-x--keymap-alist)
            (ymacs-x//create-dynamic-keymap ymacs-x-dynamic-keys)))

    (add-hook 'pre-command-hook #'ymacs-x//pre-command-hook nil t)

    (setq cursor-type 'bar)
    (setq ymacs-x--command (ymacs-x//lookup-keys ";"))
    (setq ymacs-x--activated t)))

(defun ymacs-x/deactivate ()
  (interactive)

  (when ymacs-x--activated
    (remove-hook 'pre-command-hook #'ymacs-x//pre-command-hook t)
    (setq cursor-type 'box)

    (setq ymacs-x--activated nil)))

;;;###autoload
(define-minor-mode ymacs-x-mode
  "use X as leader key"
  :group 'ymacs
  :init-value nil
  (setq ymacs-x--activated nil)

  (if ymacs-x-mode
      (progn
        (setq ymacs-x--keymap-alist
              `((ymacs-x--activated)
                (ymacs-x-mode . ,ymacs-x-keymap)))
        (add-to-list 'emulation-mode-map-alists 'ymacs-x--keymap-alist t))
    (setq ymacs-x--keymap-alist nil)
    (setq emulation-mode-map-alists (delete 'ymacs-x--keymap-alist emulation-mode-map-alists))))


(defun ymacs-x//mode-predicate ()
  (unless (minibufferp)
    (ymacs-x-mode 1)))

;;;###autoload
(define-globalized-minor-mode ymacs-x-global-mode ymacs-x-mode ymacs-x//mode-predicate)

(defun ymacs-x/warn ()
  (interactive)
  (message "Please press %s immediately after ;"
           (key-description (this-command-keys))))

(defun ymacs-x/just-x ()
  (interactive)
  (ymacs-x/deactivate)
  (call-interactively ymacs-x--command))

(defun ymacs-x/kill-or-save-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'save-buffer)))

;;;###autoload
(defun ymacs-x//enable ()
  (define-key universal-argument-map "u" #'universal-argument-more)
  (ymacs-x//set-ivy-minibuffer-map)

  (ymacs-x-global-mode 1))

(defun ymacs-x//disable ()
  (define-key universal-argument-map "u" nil)
  (ymacs-x//unset-ivy-minibuffer-map)

  (ymacs-x-global-mode -1))

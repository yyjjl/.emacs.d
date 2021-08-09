;;; -*- lexical-binding: t; -*-

(defface ymacs-x-face
  '((t :inherit mode-line-highlight :extend t))
  "color for ymacs-x indicator"
  :group 'ymacs)

(defvar ymacs-x-mode)
(defvar-local ymacs-x--keymap-alist nil)
(put 'ymacs-x--keymap-alist 'permanent-local t)

(defvar-local ymacs-x--command nil)
(defvar-local ymacs-x--return nil)
(defvar-local ymacs-x--activated nil)

(declare-function fcitx--ymacs-x-maybe-deactivate 'fcitx)
(declare-function fcitx--ymacs-x-maybe-activate 'fcitx)

(when (require 'fcitx nil t)
  (fcitx--defun-maybe "ymacs-x"))

(defvar ymacs-x-translation-table
  '((?! . ?1)
    (?@ . ?2)
    (?# . ?3)
    (?$ . ?4)
    (?% . ?5)
    (?^ . ?6)
    (?& . ?7)
    (?* . ?8)
    (?\( . ?9)
    (?\) . ?0)))

(defvar ymacs-x-dynamic-keys
  '(("'" keyboard-quit)
    ("+" "C-+")
    ("," "C-x ,")
    ("." ymacs-editor/goto-last-point :exit no)
    ("-" "C--")
    ("/" "C-/" :exit no)
    ("?" which-key-show-top-level :exit no)
    (";" ymacs-x/just-x :exit immediate)
    ("；" ymacs-x/just-x :exit immediate)
    ("=" "C-=")
    ("RET" ymacs-x/return :exit t)
    ("SPC" "C-SPC" :exit no)
    ("TAB" "C-x b")
    ("<tab>" "C-x b")
    ("[" "C-x <left>")
    ("\\" "C-c C-b")
    ("]" "C-x <right>")
    ("`" treemacs)
    ("a" "C-c i a")
    ("b" "C-c b")
    ("c" "C-c")

    ("d d" "C-k" :exit no)
    ("d k" kill-whole-line :exit no)
    ("d s" kill-sexp :exit no)
    ("d c" "C-d" :exit no)
    ("d w" "M-d" :exit no)
    ("d DEL" "C-DEL" :exit no)
    ("d <backspace>" "C-<backsapce>" :exit no)

    ("e" "C-c f l")
    ("f" "C-c C-f")
    ("g" "C-x g")
    ("h" "C-h")
    ("i" "C-c i i")
    ("j" "C-c i j")
    ("k" "C-x k")
    ("l" "C-c C-l")
    ("m" "C-x g b")
    ("n" "C-x n")
    ("o" "C-x C-f")
    ("p" "C-x p")
    ("q" ymacs-x/deactivate)
    ("r" "C-x r")
    ("s" "C-s")
    ("t" "C-x t")
    ("u" "C-u")
    ("v" "C-x v")
    ("w" ymacs-x/kill-or-save-buffer)
    ("x" "C-x")
    ("y" "C-c t")
    ("z" "C-z")))

(put 'digit-argument 'ymacs-x-exit 'no)
(put 'universal-argument 'ymacs-x-exit 'no)
(put 'universal-argument-more 'ymacs-x-exit 'no)

(defvar ymacs-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ";") #'ymacs-x/activate)
    (define-key map (kbd "；") #'ymacs-x/activate)
    map))

(defsubst ymacs-x//lookup-keys (-keys)
  (let ((ymacs-x-mode)
        (ymacs-x--activated)
        (ymacs-x--keymap-alist))
    (key-binding -keys)))

(defsubst ymacs-x//remove-modifier (-key -modifier -modifiers)
  (when (memq -modifier -modifiers)
    (event-convert-list
     (nconc (remove -modifier -modifiers)
            (list (event-basic-type -key))))))

(defun ymacs-x//translate-keymap (-keymap)
  (if (or (not (keymapp -keymap))
          (symbolp -keymap))
      -keymap
    (let ((map (make-sparse-keymap)))
      (map-keymap
       (lambda (-key -binding)
         (setq -binding (ymacs-x//translate-keymap -binding))
         (setq -key
               (or (when (integerp -key)
                     (let ((modifiers (event-modifiers -key))
                           translated-key)
                       (cond ((and (setq translated-key (alist-get -key ymacs-x-translation-table))
                                   (setq translated-key (vector translated-key))
                                   (null (lookup-key-ignore-too-long -keymap translated-key)))
                              translated-key)

                             ((and (setq translated-key
                                         (ymacs-x//remove-modifier -key 'control modifiers))
                                   (setq translated-key (vector translated-key))
                                   (null (lookup-key-ignore-too-long -keymap translated-key)))
                              translated-key)

                             ((and (null (lookup-key-ignore-too-long -keymap "'"))
                                   (setq translated-key (ymacs-x//remove-modifier -key 'shift modifiers))
                                   (setq translated-key (vector ?' translated-key)))
                              ;; keep old binding
                              (define-key map (vector -key) -binding)
                              translated-key))))
                   (vector -key)))
         ;; NOTE: parent keymap are traversed after its child
         (unless (lookup-key-ignore-too-long map -key)
           (define-key map -key -binding)))
       -keymap)
      map)))

(defun ymacs-x//undefined ()
  (interactive)
  (let* ((keys (this-command-keys))
         (translated-keys (cadr (assoc keys ymacs-x-dynamic-keys))))
    (message "%s%s is undefined"
             (key-description keys)
             (if (stringp translated-keys)
                 (concat " (simulate: " translated-keys ")")
               ""))))

(defun ymacs-x//create-dynamic-keymap (-dynamic-keys)
  (let (ymacs-x-mode
        ymacs-x--activated
        (map (make-sparse-keymap)))
    (suppress-keymap map)

    (cl-loop
     for (new-key old-key-or-def . props) in -dynamic-keys
     for binding = nil
     do (let ((exit (plist-get props :exit)))
          (if (stringp old-key-or-def)
              (progn
                (setq old-key-or-def (kbd old-key-or-def))
                (setq binding (ymacs-x//lookup-keys old-key-or-def)))
            (setq binding old-key-or-def))
          ;; keep old key-binding if exit is 'no
          (when (and (stringp old-key-or-def)
                     (not (eq exit 'no)))
            (ignore-errors (define-key map old-key-or-def #'ymacs-x//undefined)))
          (when (and binding exit (symbolp binding))
            (put binding 'ymacs-x-exit exit))

          (unless binding
            (setq binding #'ymacs-x//undefined))
          (define-key map (kbd new-key) (ymacs-x//translate-keymap binding))))
    map))

(defun ymacs-x//update-dynamic-keymap ()
  (setq-local ymacs-x--keymap-alist
              `((ymacs-x--activated . ,(ymacs-x//create-dynamic-keymap ymacs-x-dynamic-keys))
                (ymacs-x-mode . ,ymacs-x-keymap))))

(defun ymacs-x//pre-command-hook ()
  (let ((exit (and (symbolp this-command)
                   (get this-command 'ymacs-x-exit)))
        (after-activated (eq real-last-command 'ymacs-x/activate)))
    (if (and (not after-activated)
             (eq exit 'immediate)
             (not (eq real-last-command this-command)))
        (setq this-command #'ymacs-x/warn)
      (when (or (eq exit t)
                (and after-activated
                     (not (eq exit 'no))))
        (ymacs-x/deactivate)))))

(defun ymacs-x/activate ()
  (interactive)

  (unless ymacs-x--activated
    (unless (assq 'ymacs-x--activated ymacs-x--keymap-alist)
      (ymacs-x//update-dynamic-keymap))

    (add-hook 'pre-command-hook #'ymacs-x//pre-command-hook nil t)

    (ignore-errors (fcitx--ymacs-x-maybe-deactivate))
    ;; (setq cursor-type 'bar)
    (setq ymacs-x--command (ymacs-x//lookup-keys ";"))
    (setq ymacs-x--return (ymacs-x//lookup-keys (kbd "RET")))
    (setq ymacs-x--activated t)

    (unless (display-graphic-p)
      (force-mode-line-update))))

(defun ymacs-x/deactivate ()
  (interactive)

  (when ymacs-x--activated
    (remove-hook 'pre-command-hook #'ymacs-x//pre-command-hook t)
    ;; (setq cursor-type 'box)

    (ignore-errors (fcitx--ymacs-x-maybe-activate))

    (setq ymacs-x--activated nil)

    (unless (display-graphic-p)
      (force-mode-line-update))))

(define-minor-mode ymacs-x-mode
  "use X as leader key"
  :group 'ymacs-x
  :init-value nil
  (setq ymacs-x--activated nil)
  (if ymacs-x-mode
      (progn
        (setq ymacs-x--keymap-alist
              `((ymacs-x-mode . ,ymacs-x-keymap)))
        (add-to-list 'emulation-mode-map-alists 'ymacs-x--keymap-alist t))
    (setq ymacs-x--keymap-alist nil)
    (setq emulation-mode-map-alists (delete 'ymacs-x--keymap-alist emulation-mode-map-alists))))

(defvar ymacs-x-indicator-overlay nil)

(defun ymacs-x//post-command-hook ()
  (if ymacs-x--activated
      (let ((start (line-beginning-position))
            (end (min (point-max) (1+ (line-end-position)))))
        (if (overlayp ymacs-x-indicator-overlay)
            (move-overlay ymacs-x-indicator-overlay start end (current-buffer))
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face 'ymacs-x-face)
            (overlay-put ov 'priority 100)
            (setq ymacs-x-indicator-overlay ov))))
    (when (overlayp ymacs-x-indicator-overlay)
      (delete-overlay ymacs-x-indicator-overlay)
      (setq ymacs-x-indicator-overlay nil))))

(defun ymacs-x//mode-predicate ()
  (unless (minibufferp)
    (ymacs-x-mode 1)))

(define-globalized-minor-mode ymacs-x-global-mode ymacs-x-mode ymacs-x//mode-predicate
  :group 'ymacs-x
  (if ymacs-x-global-mode
      (add-hook 'post-command-hook #'ymacs-x//post-command-hook)
    (remove-hook 'post-command-hook #'ymacs-x//post-command-hook)))

(defun ymacs-x/warn ()
  (interactive)
  (message "Please press %s again" (key-description (this-command-keys))))

(defun ymacs-x/just-x ()
  (interactive)
  (ymacs-x/deactivate)
  (call-interactively ymacs-x--command))

(defun ymacs-x/return ()
  (interactive)
  (ymacs-x/deactivate)
  (when (eq real-last-command 'ymacs-x/activate)
    (self-insert-command 1 ?\;))
  (call-interactively ymacs-x--return))

(defun ymacs-x/kill-or-save-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'save-buffer)))

(after! which-key
  (put 'which-key-C-h-dispatch 'ymacs-x-exit 'no)
  (map-keymap
   (lambda (_key def)
     (when (and (symbolp def) (commandp def))
       (put def 'ymacs-x-exit 'no)))
   which-key-C-h-map))

(defun ymacs-x//enable ()
  (define-key universal-argument-map "u" #'universal-argument-more)

  (advice-add 'wgrep-change-to-wgrep-mode :after #'ymacs-x//update-dynamic-keymap)
  (ymacs-x-global-mode 1))

(defun ymacs-x//disable ()
  (define-key universal-argument-map "u" nil)

  (advice-remove 'wgrep-change-to-wgrep-mode #'ymacs-x//update-dynamic-keymap)
  (ymacs-x-global-mode -1))


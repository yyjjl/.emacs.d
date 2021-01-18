;; -*- lexical-binding: t; -*-

(defcustom ymacs-x-char ";"
  "."
  :group 'ymacs
  :type 'string)

(defvar-local ymacs-x-dynamic-keymap nil)
(defvar ymacs-x--cursor-background nil)
(defvar ymacs-x--activated nil)

(quail-define-package
 "quote"                                ;name
 "UTF-8"                                ;language
 "'"                                    ;title (in mode line)
 nil                                      ;guidance
 nil                                    ;docstring
 nil                                    ;translation-keys
 nil                                    ;forget-last-selection
 nil                                    ;deterministic
 nil                                    ;kbd-translate
 nil                                    ;show-layout
 nil                                    ;create-decode-map
 nil                                    ;maximum-shortest
 nil                                    ;overlay-plist
 nil                                    ;update-translation-function
 nil                                    ;conversion-keys
 t                                      ;simple
 )

(quail-define-rules
 ("''" "'")

 ("'a" "A")
 ("'b" "B")
 ("'c" "C")
 ("'d" "D")
 ("'e" "E")
 ("'f" "F")
 ("'g" "G")
 ("'h" "H")
 ("'i" "I")
 ("'j" "J")
 ("'k" "K")
 ("'l" "L")
 ("'m" "M")
 ("'n" "N")
 ("'o" "O")
 ("'p" "P")
 ("'q" "Q")
 ("'r" "R")
 ("'s" "S")
 ("'t" "T")
 ("'u" "U")
 ("'v" "V")
 ("'w" "W")
 ("'x" "X")
 ("'y" "Y")
 ("'z" "Z")

 ("'`" "~")
 ("'1" "!")
 ("'2" "@")
 ("'3" "#")
 ("'4" "$")
 ("'5" "%")
 ("'6" "^")
 ("'7" "&")
 ("'8" "*")
 ("'9" "(")
 ("'0" ")")
 ("'-" "_")
 ("'=" "+")
 ("'[" "{")
 ("']" "}")
 ("'\\" "|")
 ("';" ":")
 ("' " "\"")
 ("'." ">")
 ("'," "<")
 ("'/" "?"))

(defvar ymacs-x-dynmaic-keys
  `((,ymacs-x-char ymacs-x/just-x)
    ("SPC" "C-SPC")
    ("TAB" ymacs-git/status)
    ("'" ymacs-x/keyboard-quit)
    ("[" "C-x <left>")
    ("]" "C-x <right>")
    ("+" "C-+")
    ("-" "C--")
    ("=" "C-=")
    ("," "C-x ,")
    ("." "C-x b")
    ;; ("/")
    ;; ("\\")

    ("a" "C-c i a")
    ("b" "C-c B")
    ("c" "C-c")
    ("d" "C-c C-d")
    ("e" "C-c e")
    ("f" "C-x C-f")
    ("g" "M-g")
    ("h" "C-h")
    ("i" "C-c i")
    ("j" "C-c i j")
    ("k" "C-x k")
    ("l" "C-c C-l")
    ("m" "C-c C-SPC")
    ("n" "C-x n")
    ("o" "C-S-t")
    ("p" "C-c C-b")
    ("q" ymacs-x/deactivate)
    ("r" "C-c C-z")
    ("s" "C-s")
    ("t" "C-c t")
    ("u" "C-u")
    ("v" "C-x v")
    ("w" "C-x C-s")
    ("x" "C-x")
    ;; ("y")
    ("z" "C-z")))

(defvar ymacs-x-dynmaic-minibuffer-keys
  `(("SPC" "C-SPC")
    ("n" "C-n")
    ("p" "C-p")
    ("l" "C-l")
    ("k" "C-k")
    ("o" "C-c C-o")
    ("q" ymacs-x/deactivate)
    (,ymacs-x-char ymacs-x/just-x)
    ("'" ymacs-x/keyboard-quit)))

(defvar ymacs-x-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ymacs-x-char) #'ymacs-x/activate)
    (define-key map (kbd "M-[") #'ymacs-x/activate)
    map))

(add-variable-watcher
 'ymacs-x-char
 (lambda (-sym -new-value -op _where)
   (let ((old-value (symbol-value -sym)))
     (when (and (eq -op 'set)
                (not (equal old-value -new-value)))
       (define-key ymacs-x-keymap (kbd old-value) nil)
       (define-key ymacs-x-keymap (kbd -new-value) #'ymacs-x/activate)))))

(defsubst ymacs-x//simulate-keys (-keys)
  (setq -keys (listify-key-sequence (if (stringp -keys)
                                        (kbd -keys)
                                      -keys)))
  (setq unread-command-events
        (append (cl-loop for key in -keys
                         collect (if (consp key)
                                     key
                                   (cons t key)))
                unread-command-events)))

(defun ymacs-x//create-dynamic-keymap (-dynamic-keys)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (cl-loop
     for i from 0 to 127
     when (not (or (= i 7)
                   (and (>= i ?0) (<= i ?9))))
     do (define-key map (char-to-string i) 'undefined))

    (cl-loop
     for (new-key old-key-or-def) in -dynamic-keys
     for binding = nil
     do (progn
          (if (stringp old-key-or-def)
              (progn
                (setq old-key-or-def (kbd old-key-or-def))
                (setq binding (key-binding old-key-or-def)))
            (setq binding old-key-or-def))
          (when binding
            (when (stringp old-key-or-def)
              (ignore-errors (define-key map old-key-or-def 'undefined)))
            (define-key map (kbd new-key) binding))))
    map))

;;;###autoload
(define-minor-mode ymacs-x-mode
  "use X as leader key"
  :group 'ymacs
  :init-value nil
  (if ymacs-x-mode
      (activate-input-method "quote")
    (when (equal current-input-method "quote")
      (activate-input-method nil)))
  (setq ymacs-x-dynamic-keymap nil)
  (modify-local-minor-mode-map!
   'ymacs-x-mode
   (and ymacs-x-mode ymacs-x-keymap)))

;;;###autoload
(define-globalized-minor-mode ymacs-x-global-mode ymacs-x-mode ymacs-x-mode)

(defun ymacs-x//keep-state-p ()
  (or (eq 'no (get this-command 'ymacs-x-exit))
      (and (minibufferp)
           (eq this-command 'undefined))))

(defun ymacs-x//cleanup ()
  (setq ymacs-x--activated nil)
  (set-face-attribute 'cursor nil :background ymacs-x--cursor-background))

(defun ymacs-x/activate ()
  (interactive)
  (if ymacs-x--activated
      (progn
        (setq real-this-command real-last-command)
        (setq this-command last-command))
    (unless ymacs-x-dynamic-keymap
      (setq ymacs-x-dynamic-keymap
            (ymacs-x//create-dynamic-keymap
             (if (minibufferp)
                 ymacs-x-dynmaic-minibuffer-keys
               ymacs-x-dynmaic-keys))))

    (setq ymacs-x--activated t)
    (setq ymacs-x--cursor-background (face-attribute 'cursor :background))
    (set-face-attribute 'cursor nil :background "darkgreen")

    (set-transient-map
     ymacs-x-dynamic-keymap
     #'ymacs-x//keep-state-p
     #'ymacs-x//cleanup)))

(defun ymacs-x/deactivate ()
  (interactive)
  (ymacs-x//cleanup))

(defun ymacs-x//try-execute-command (-keys)
  (prefix-command-preserve-state)
  (setq prefix-arg current-prefix-arg)

  (let* (ymacs-x-mode
         (binding (key-binding (kbd -keys))))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd -keys) binding)
       map))
    (ymacs-x//simulate-keys -keys)))

(defun ymacs-x/just-x ()
  (interactive)
  (ymacs-x//try-execute-command ymacs-x-char))

(defun ymacs-x/keyboard-quit ()
  (interactive)
  (setq this-command 'keyboard-quit)
  (setq real-this-command 'keyboard-quit)
  (setq unread-command-events
        (append '((t . 7)) unread-command-events)))

(put 'digit-argument 'ymacs-x-exit 'no)
(put 'universal-argument 'ymacs-x-exit 'no)
(put 'universal-argument-more 'ymacs-x-exit 'no)
(put 'scroll-up-command 'ymacs-x-exit 'no)

(put 'ivy-next-line 'ymacs-x-exit 'no)
(put 'ivy-previous-line 'ymacs-x-exit 'no)
(put 'ivy-next-line-and-call 'ymacs-x-exit 'no)
(put 'ivy-previous-line-and-call 'ymacs-x-exit 'no)
(put 'swiper-recenter-top-bottom 'ymacs-x-exit 'no)
(put 'ivy-call-and-recenter 'ymacs-x-exit 'no)

(after! 'ivy
  (define-key! :map ivy-minibuffer-map
    ("M-g" . ymacs-x/keyboard-quit)))

;;;###autoload
(defun ymacs-x//enable ()
  (define-key universal-argument-map "u" #'universal-argument-more)

  (ymacs-x-global-mode 1))

(defun ymacs-x//disable ()
  (define-key universal-argument-map "u" nil)

  (ymacs-x-global-mode -1))

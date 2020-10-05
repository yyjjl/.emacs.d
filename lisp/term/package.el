;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'dash))

(define-option! ymacs-term-type 'vterm)

(executable! zsh)
(executable! bash)

(require-packages!
 (vterm :when (eq ymacs-term-type 'vterm))
 bash-completion)

;; kill the buffer when terminal is exited
(defvar ymacs-term-directory-functions '(projectile-project-root))
(defvar ymacs-term-environment-functions ())

(defvar ymacs-term-initial-directory "~/")
(defvar ymacs-term-program-arguments ())

(defvar-local ymacs-term-extra-name nil)
(defvar-local ymacs-term-exit-action 'kill
  "shell => when process exits, switch to other shell
keep   => when process exits, make buffer readonly
kill     => when process exits, kill buffer")
(put 'ymacs-term-exit-action 'permanent-local t)

(defvar ymacs-term-or-comint-process-exit-hook '(ymacs-term|shell-exit))

(defvar-local ymacs-term--ssh-info nil)
(defvar-local ymacs-term--parent-buffer nil)

(defvar ymacs-term-buffer-name "shell"
  "The buffer name of term buffer.")

(defvar ymacs-term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "C-u")
  "The key list that will need to be unbind.")

(defconst ymacs-term--setup-dir-tracking-alist
  '((term
     term-send-raw-string
     ("zsh" . " chpwd() { printf '\\033AnSiTu %s\\n' \"$USER\"; print -P '\\033AnSiTc %d'; }; clear\n"))
    (vterm
     vterm-send-string
     ("zsh" . " chpwd() { print -Pn '\\e]51;A$(pwd)\\e\\\\'; }; clear\n")
     ("bash" . " cd() { builtin cd \"$@\" || return; [ \"$OLDPWD\" = \"$PWD\" ] || echo -e \"\\e]51;A$(pwd)\\e\\\\\"; }; clear\n"))))

(defvar ymacs-term-bind-key-alist
  `(("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . ymacs-term/send-esc)
    ("C-c C-l" . term-line-mode)
    ;; ("C-n" . next-line)
    ("C-/" . ymacs-term/send-undo)
    ("M-c" . ymacs-term/send-M-c)
    ("M-u" . ymacs-term/send-M-u)
    ("M-l" . ymacs-term/send-M-l)
    ("C-m" . ymacs-term/send-return)
    ("C-y" . term-paste)
    ("C-k" . ymacs-term/kill-line)
    ("M-f" . ymacs-term/send-forward-word)
    ("M-b" . ymacs-term/send-backward-word)
    ("M-p" . ymacs-term/send-up)
    ("M-n" . ymacs-term/send-down)
    ("<C-backspace>" . ymacs-term/send-backward-kill-word)
    ("<M-backspace>" . ymacs-term/send-backward-kill-word)
    ("C-DEL" . ymacs-term/send-backward-kill-word)
    ("M-DEL" . ymacs-term/send-backward-kill-word)
    ("M-r" . ymacs-term/send-reverse-search-history)
    ("M-d" . ymacs-term/send-delete-word)
    ("M-," . term-send-raw)
    ("M-y" . ymacs-term/yank-pop)
    ("C-s" . ymacs-term/swiper)
    ("M-}" . ymacs-term/next)
    ("M-{" . ymacs-term/prev)
    ("M-o" . ymacs-term/switch)
    ("M-N" . ymacs-term/set-extra-name)
    ("C-S-t" . ymacs-term/pop-shell-current-directory)
    ("C-g" . keyboard-quit))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format.")

(define-key!
  ([f8] . ymacs-term/pop-shell))

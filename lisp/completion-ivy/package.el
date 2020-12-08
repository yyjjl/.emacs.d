;; -*- lexical-binding: t -*-

(executable! ripgrep :exe "rg")

(defvar ymacs-ivy-switch-function-list nil)

(defvar ymacs-ivy-rg-type-aliases
  (eval-when-compile
    (ignore-errors
      (append
       (--map
        (-let* (((type alias) (split-string it ":" :omit-nulls)))
          (cons (string-trim type)
                (mapcar #'string-trim (split-string alias "," :omit-nulls))))
        (-> counsel-rg-base-command
            split-string
            car
            (concat " --type-list")
            shell-command-to-string
            (split-string "\n" :omit-nulls)))
       '(("all" "all defined type aliases") ;; rg --type=all
         ("everything" "*"))))))            ;; rg without '--type'

(defvar ymacs-ivy-grep-help-commands
  '((counsel-git-grep-query-replace . "query-replace")
    (swiper-avy . "avy-jump")
    (counsel-cd . "cd")
    (ymacs-ivy/select-rg-type-aliases . "select type")))

(define-key!
  ("C-x j b" . counsel-bookmark)
  ("C-s" . ymacs/swiper)
  ("C-r" . swiper-isearch-backward)
  ("C-x C-f" . counsel-find-file)
  ("C-x k" . ymacs-counsel/kill-buffer)
  ("C-x b" . ivy-switch-buffer)
  ("C-x w -" . ivy-pop-view)
  ("C-x w =" . ivy-push-view))

(define-key! :prefix "C-c i"
  ([remap compile] . counsel-compile)
  ("r" . ivy-resume)
  ("l l" . counsel-load-library)
  ("l t" . counsel-load-theme)
  ("l p" . counsel-list-processes)
  ("l f" . counsel-find-library)
  ("u" . counsel-unicode-char)
  ("d" . counsel-dired-jump)
  ("i" . ymacs-counsel/semantic-or-imenu)
  ("x" . counsel-linux-app)
  ("v" . counsel-set-variable)
  ("j" . counsel-fzf)
  ("g" . counsel-git)
  ("s" . counsel-git-grep)
  ("S" . counsel-git-stash)
  ("h" . counsel-minibuffer-history)
  ("m" . counsel-mark-ring)
  ("/" . counsel-grep)
  ("L" . counsel-locate)
  ("f" . counsel-describe-face)
  ("S" . counsel-find-symbol)
  ("F" . counsel-faces)
  ("W" . counsel-colors-web)
  ("E" . counsel-colors-emacs)
  ("e" . sudo-edit)
  ("O" . counsel-outline)
  ("o" . counsel-org-goto-all)
  ("t" . counsel-tmm))

(define-key! :map read-expression-map
  ("C-r" . counsel-minibuffer-history))

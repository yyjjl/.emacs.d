;; -*- lexical-binding: t -*-

(executable! ripgrep :exe "rg")

(require 'transient)

(defclass transient-option-with-default (transient-option)
  ((value :initarg :value)))

(defclass transient-option-path (transient-option-with-default) ())

(cl-defmethod transient-init-value ((obj transient-option-with-default))
  (oset obj value (format "%s" (oref obj value))))

(cl-defmethod transient-format-value :around ((obj transient-option-with-default))
  (let ((choices (oref obj choices))
        (value (oref obj value)))
    (if choices
        (format
         (propertize
          (format "[%s]"
                  (mapconcat (lambda (choice) (if (equal choice value) "%s" choice))
                             choices
                             "|"))
          'face 'transient-inactive-value)
         (propertize value 'face 'font-lock-warning-face))
      (cl-call-next-method))))

(cl-defmethod transient-infix-read ((obj transient-option-with-default))
  (let ((choices (oref obj choices)))
    (if choices
        (or (when-let ((value (oref obj value)))
              (cadr (member value choices)))
            (car choices))
      (cl-call-next-method))))

(cl-defmethod transient-infix-read ((obj transient-option-path))
  (let ((default-directory (oref obj value)))
    (condition-case nil
        (read-directory-name "Path=" nil nil :must-match)
      (quit default-directory))))



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
  ("C-x j j" . counsel-bookmark)
  ("C-s" . ymacs/swiper)
  ("C-r" . swiper-isearch-backward)
  ("C-x C-f" . counsel-find-file)
  ("C-x k" . ymacs-counsel/kill-buffer)
  ("C-x b" . ivy-switch-buffer)
  ("C-x w -" . ivy-pop-view)
  ("C-x w =" . ivy-push-view))

(define-key! :prefix "C-c i"
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

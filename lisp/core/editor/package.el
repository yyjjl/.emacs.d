;;; -*- lexical-binding: t; -*-

(executable! gls)
(executable! ripgrep :-exe "rg")
(executable! fdfind :-exe ["fdfind" "fd"])
(executable! fcitx :-exe "fcitx-remote")
(executable! ctags :-exe "ctags-exuberant")

(defcustom ymacs-editor-use-childframe nil
  "Whether to use childframe"
  :group 'ymacs
  :type 'boolean)

(eval-when-compile-config!
 (when sys/macp
   (message "You need go `https://github.com/xcodebuild/fcitx-remote-for-osx' to get fcitx support"))

 (when (and ymacs-ripgrep-path
            (not (eq (shell-command
                      (concat ymacs-ripgrep-path " --pcre2-version"))
                     0)))
   (message "You need install ripgrep with pcre2 support (@see %s)" (expand-etc! "setup/install_rust.sh"))))

(require-packages!
 (exec-path-from-shell :when sys/macp)
 (fcitx :when (and sys/linuxp ymacs-fcitx-path))
 ;; `counsel-M-x' need amx to get history
 amx
 company
 (company-posframe :when ymacs-editor-use-childframe)
 ivy
 ivy-hydra
 hydra
 pretty-hydra
 counsel
 flycheck
 projectile
 swiper
 yasnippet
 yasnippet-snippets
 dumb-jump
 expand-region
 easy-kill
 paredit
 goto-chg
 ;; Mark tools
 multiple-cursors
 ;; `wgrep' allows you to edit a grep buffer and apply those changes
 ;; to the file buffer.
 wgrep
 buffer-move
 graphviz-dot-mode)

(defvar ymacs-editor-toggles-alist
  '(("Global"
     (t
      ("V"
       (ymacs-ui/view-code-mode (if ymacs-ui/view-code-mode -1 1))
       "View Code"
       :toggle ymacs-ui/view-code-mode)
      ("E"
       toggle-debug-on-error
       "Debug on Error"
       :toggle (default-value 'debug-on-error))
      ("Q"
       toggle-debug-on-quit
       "Debug on Quit"
       :toggle (default-value 'debug-on-quit))
      ("W"
       (setq show-trailing-whitespace (not show-trailing-whitespace))
       "Trailing Whitespace"
       :toggle show-trailing-whitespace)
      ("N"
       (display-line-numbers-mode (if display-line-numbers-mode -1 1))
       "Line Number"
       :toggle display-line-numbers-mode)
      ("B" display-battery-mode "Battery" :toggle t)
      ("T" display-time-mode "Time" :toggle t)
      ("P" prettify-symbols-mode "Pretty Symbol" :toggle t)))))

(defvar ymacs-editor-narrow-dwim-alist nil)

(defvar ymacs-editor-ivy-switch-function-list nil)
(defvar ymacs-editor-ivy-extra-environment nil)
(defvar ymacs-editor-ivy-display-help-max-width 160)

(defvar ymacs-editor-ivy-display-help-extra-commands
  '(ivy-restrict-to-matches
    delete-blank-lines
    just-one-space
    (counsel-find-file . ivy-magic-read-file-env)))

(defvar ymacs-editor-ivy-display-help-ignore-commands
  '(ymacs-editor/ivy-meta-dot))

(defvar ymacs-editor-rg-type-aliases
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
         ("everything" "*"))))))

(defvar ymacs-editor-hs-overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defvar ymacs-editor-projectile-invalidate-cache-empty-vars
  '(ymacs-modeline--buffer-file-name
    ymacs-modeline--vcs-state
    ymacs-modeline--project-detected-p
    ymacs-modeline--project-root))

(defface ymacs-editor-hs-overlay-face
  '((t (:inherit font-lock-builtin-face :underline t)))
  "Face used for the dirname part of the buffer path."
  :group 'hideshow)

(defcustom ymacs-editor-local-snippets-list nil
  "local snippets"
  :group 'ymacs
  :type '(alist :key-type string :value-type string)
  :safe #'listp)

(defcustom ymacs-editor-project-rsync-remote-path nil
  "."
  :group 'ymacs
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-editor-project-rsync-local-path nil
  "."
  :group 'ymacs
  :type '(directory :must-match t)
  :safe #'file-directory-p)

(defcustom ymacs-editor-project-rsync-extra-options nil
  "."
  :group 'ymacs
  :type '(repeat string)
  :safe (lambda (x) (and (listp x) (-all? #'stringp x))))

(defvar ymacs-editor-project-rsync-command
  "rsync -azh --progress --filter=':- .gitignore' %s %s %s")

(defvar-local ymacs-editor-compile-command-functions nil)
(defvar ymacs-editor-environment-functions ())

(defvar ymacs-editor-surround-pair-alist
  '(("()" . ("(" . ")"))
    ("{}" . ("{" . "}"))
    ("[]" . ("[" . "]"))
    ("`" . (lambda (_)
             (cons "`"
                   (if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                       "'"
                     "`"))))
    ("<>,." . (lambda (_)
                (let ((tag (read-string "Tag: ")))
                  (cons (concat "<" tag ">")
                        (concat "</" tag ">")))))
    ("\\" . (lambda (_)
              (let ((pair (ymacs-editor//surround-get-pair (read-char))))
                (when pair
                  (cons (concat "\\" (car pair))
                        (concat "\\" (cdr pair)))))))
    ("b" . (lambda (_)
             (let ((env (read-string "environment: ")))
               (when env
                 (cons (concat "\\begin{" env "}")
                       (concat "\\end{" env "}"))))))))

(defvar ymacs-editor-avy-copy-key-alist
  '((?s . symbol)
    (?e . sexp)
    (?l . line)
    (?f . filename)
    (?d . defun)
    (?W . word)
    (?u . url)
    (?U . uuid)
    (?n . number)))

(put 'ymacs-editor//fzf 'no-counsel-M-x t)
(put 'ymacs-editor//rg 'no-counsel-M-x t)
(put 'ymacs-editor//rg-in-directory 'no-counsel-M-x t)
(put 'ymacs-editor/fzf-file-type 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot-for-counsel-fzf 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot-for-counsel-rg 'no-counsel-M-x t)
(put 'ymacs-editor/switch-ivy-backend 'no-counsel-M-x t)

(autoload 'ymacs-editor/rg (expand! "commands-ivy.el") nil t)
(autoload 'ymacs-editor/fzf (expand! "commands-ivy.el") nil t)

(autoload 'ymacs-hydra/mc/mc/mark-next-like-this (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/mc/mc/mark-previous-like-this (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/ediff/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/next-error/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/outline/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/rectangle/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/enlarge-window (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/enlarge-window-horizontally (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/shrink-window (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/shrink-window-horizontally (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/sort/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/games/body (expand! "commands-hydra") nil t)

(autoload 'paredit-kill "paredit" nil t)
(autoload 'paredit-backward-delete "paredit" nil t)
(autoload 'paredit-forward-delete "paredit" nil t)
(autoload 'paredit-backward-kill-word "paredit" nil t)
(autoload 'paredit-forward-kill-word "paredit" nil t)
(autoload 'paredit-close-round "paredit" nil t)
(autoload 'paredit-close-square "paredit" nil t)
(autoload 'paredit-close-curly "paredit" nil t)

(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)

(define-key! :prefix "C-x w"
  ;; buffer-mode
  ("k" . buf-move-up)
  ("j" . buf-move-down)
  ("h" . buf-move-left)
  ("l" . buf-move-right)

  ("-" . ivy-pop-view)
  ("=" . ivy-push-view)
  ("." . ivy-switch-view))

(define-key! :prefix "C-x"
  ("C-_" . goto-last-change)
  ("C-/" . goto-last-change)

  ("o" . ace-window)
  ("b" . ivy-switch-buffer)
  ("k" . ymacs-editor/kill-buffer)
  ("K" . ymacs-editor/kill-regexp)

  ("j b" . counsel-bookmark)
  ("n n" . ymacs-editor/narrow-or-widen-dwim)

  ("{" . ymacs-hydra/window/shrink-window-horizontally)
  ("}" . ymacs-hydra/window/enlarge-window-horizontally)
  ("^" . ymacs-hydra/window/enlarge-window)
  ("-" . ymacs-hydra/window/shrink-window)
  ("?" . ymacs-hydra/window/body)
  ("`" . ymacs-hydra/next-error/body)
  ("SPC" . ymacs-hydra/rectangle/body)

  (", s" . ymacs-hydra/sort/body)
  (", e" . ymacs-hydra/ediff/body)
  (", z" . ymacs-hydra/games/body)
  ;; Minor mode to make xref use etags again.
  (", E" . xref-etags-mode)

  (", SPC" . ymacs-editor/insert-space-around-chinese)
  (", a" . ymacs-editor/add-local-snippet)
  (", d" . ymacs-editor/delete-local-snippet))

(define-key!
  ([remap kill-ring-save] . easy-kill)

  ("C-c O" . ymacs-hydra/outline/body)

  ("C-c <tab>" . company-complete)
  ("C-c TAB" . company-complete)
  ("C-c F" . company-files)
  ("C-}" . company-yasnippet)

  ("C-s" . ymacs-editor/swiper)
  ("C-r" . swiper-isearch-backward)

  ("M--" . er/expand-region)
  ("M-i" . ymacs-editor/iedit-mode)
  ("M-I" . iedit-rectangle-mode)

  ("M-k" . kill-sexp)
  ("C-M-k" . kill-sentence)

  ("M-Q" . ymacs-editor/insert-space-around-chinese)
  ("M-;" . ymacs-editor/comment-dwim)
  ("M-}" . ymacs-editor/forward-defun)
  ("M-{" . ymacs-editor/backward-defun)
  ("M-e" . ymacs-editor/forward-sentence-or-sexp)
  ("M-a" . ymacs-editor/backward-sentence-or-sexp)
  ("C-M-b" . backward-sentence)
  ("C-M-f" . forward-sentence)

  ("C-=" . ymacs-hydra/mc/mc/mark-next-like-this)
  ("C--" . ymacs-hydra/mc/mc/mark-previous-like-this)
  ("C->" . ymacs-hydra/mc/mc/mark-next-like-this)
  ("C-<" . ymacs-hydra/mc/mc/mark-previous-like-this)

  ("M-0" . ymacs-editor/goto-char-or-minibuffer)
  ("M-7" . ymacs-editor/avy-copy-and-yank)
  ("M-8" . avy-goto-word-or-subword-1)
  ("M-9" . ymacs-editor/avy-goto-subword-1-in-defun)
  ("M-g w" . ymacs-editor/avy-copy)
  ("M-g y" . ymacs-editor/avy-copy-and-yank)
  ("M-g 1" . avy-goto-char)
  ("M-g 2" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g s" . avy-goto-symbol-1)
  ("M-g L" . avy-copy-line)

  ("M-'" . ymacs-editor/change-surround)

  ([remap move-beginning-of-line] . ymacs-editor/smart-move-begining-of-line)

  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)
  ([f6] . ymacs-editor/toggle-company-ispell)
  ([C-f7] . ymacs-editor/rsync-project)
  ([f7] . ymacs-hydra/toggles)
  ([f10] . counsel-compile))

(define-key! :prefix "C-c m"
  ("P" . mc/mark-pop)
  ("m" . mc/mark-all-like-this-dwim)
  ("l" . mc/edit-lines)
  ("t" . mc/mark-sgml-tag-pair)
  ("e" . mc/edit-ends-of-lines)
  ("a" . mc/edit-beginnings-of-lines)
  ("i" . mc/insert-numbers)
  ("L" . mc/insert-letters)
  ("s" . mc/sort-regions)
  ("v" . mc/vertical-align)
  ("r" . mc/reverse-regions)
  ("." . ymacs-hydra/mc/mc/mark-next-like-this)
  ("," . ymacs-hydra/mc/mc/mark-previous-like-this)
  ([C-S-mouse-1] . mc/add-cursor-on-click))

(define-key! :prefix "C-c i"
  ("r" . ivy-resume)
  ("e" . ymacs-editor/find-file-as-root)

 ([remap compile] . counsel-compile)
  ([remap dired] . counsel-dired)
  ([remap insert-char] . counsel-unicode-char)
  ([remap set-variable] . counsel-set-variable)

  ("j" . ymacs-editor/fzf)
  ("a" . ymacs-editor/rg)
  ("i" . ymacs-editor/semantic-or-imenu)

  ("l l" . counsel-load-library)
  ("l t" . counsel-load-theme)
  ("l p" . counsel-list-processes)
  ("l f" . counsel-find-library)
  ("u" . counsel-unicode-char)
  ("d" . counsel-dired-jump)
  ("x" . counsel-linux-app)
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
  ("O" . counsel-outline)
  ("o" . counsel-org-goto-all)
  ("t" . counsel-tmm))

(define-key! :map prog-mode-map
  ([remap kill-line] . paredit-kill)
  ([remap delete-char] . paredit-forward-delete)
  ([remap delete-backward-char] . paredit-backward-delete)
  ([remap kill-word] . paredit-forward-kill-word)
  ([remap backward-kill-word] . paredit-backward-kill-word)

  ("]" . paredit-close-square)
  (")" . paredit-close-round)
  ("}" . paredit-close-curly))

(define-key! :map read-expression-map
  ("C-r" . counsel-minibuffer-history))

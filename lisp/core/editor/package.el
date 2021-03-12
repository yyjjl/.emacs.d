;;; -*- lexical-binding: t; -*-

(executable! gls)
(executable! ripgrep :-exe "rg")
(executable! fdfind :-exe ["fdfind" "fd"])
(executable! ctags :-exe "ctags-exuberant")

(option! editor-use-childframe nil
  "Whether to use childframe"
  :type 'boolean)

(option! default-project nil
  :type 'list
  :safe 'consp)

(eval-when-compile-config!
 (when sys/macp
   (message "You need go `https://github.com/xcodebuild/fcitx-remote-for-osx' to get fcitx support"))

 (when (and ymacs-ripgrep-path
            (not (eq (shell-command
                      (concat ymacs-ripgrep-path " --pcre2-version"))
                     0)))
   (message "You need install ripgrep with pcre2 support (@see %s)"
            (expand-etc! "setup/install_rust.sh"))))

(require-packages!
 (exec-path-from-shell :when sys/macp)
 (fcitx :when sys/linuxp)
 ;; `counsel-M-x' need amx to get history
 amx
 company
 (company-posframe :when ymacs-editor-use-childframe-p)
 ivy
 ivy-hydra
 pretty-hydra
 counsel
 swiper
 yasnippet
 yasnippet-snippets
 expand-region
 goto-chg
 multiple-cursors
 wgrep
 ace-pinyin
 ace-window
 persistent-scratch
 hl-todo
 page-break-lines
 which-key
 highlight-indentation)

(put 'ymacs-editor//fzf 'no-counsel-M-x t)
(put 'ymacs-editor//rg 'no-counsel-M-x t)
(put 'ymacs-editor//rg-in-directory 'no-counsel-M-x t)
(put 'ymacs-editor/fzf-file-type 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot-for-counsel-fzf 'no-counsel-M-x t)
(put 'ymacs-editor/ivy-meta-dot-for-counsel-rg 'no-counsel-M-x t)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

(autoload 'ymacs-editor/rg (expand! "commands-ivy.el") nil t)
(autoload 'ymacs-editor/fzf (expand! "commands-ivy.el") nil t)

(autoload 'ymacs-hydra/mc/mc/mark-next-like-this (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/mc/mc/mark-previous-like-this (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/ediff/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/outline/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/rectangle/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/enlarge-window (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/enlarge-window-horizontally (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/shrink-window (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/shrink-window-horizontally (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/window/body (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/sort/body (expand! "commands-hydra") nil t)

(autoload 'ymacs-editor/generate-autoloads (expand! "commands-package") nil t)

(autoload 'xref-pulse-momentarily "xref")
(autoload 'project-root "project")
(autoload 'project-current "project")
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'pinyinlib-build-regexp-string "pinyinlib")
(autoload 'winner-undo "winner" nil t)
(autoload 'winner-redo "winner" nil t)

(define-key! :map indent-rigidly-map
  ("[" . indent-rigidly-left)
  ("]" . indent-rigidly-right)
  ("{" . indent-rigidly-left-to-tab-stop)
  ("}" . indent-rigidly-right-to-tab-stop))

(define-key! :map special-mode-map
  ("u" . scroll-down-command)
  ("y" . scroll-down-line)
  ("e" . scroll-up-line))

(define-key! :prefix "C-x"
  ("'")                                 ; unbind

  ("c" . ymacs-editor/cleanup-buffer-safe)
  (", -" . ymacs-editor/copy-file-name)

  ("R" . ymacs-editor/rename-this-file-and-buffer)
  ("W" . ymacs-editor/copy-this-file)
  ("D" . ymacs-editor/delete-this-file)

  ("G" . revert-buffer)
  ("I" . clone-indirect-buffer)

  ("w [" . winner-undo)
  ("w ]" . winner-redo)

  ("2" . ymacs-editor/window-split-vertically)
  ("3" . ymacs-editor/window-split-horizontally)
  ("|" . ymacs-editor/window-force-split-horizontally)
  ("_" . ymacs-editor/window-force-split-vertically)

  ("w w" . ymacs-editor/move-buffer)

  (("C-b" "B") . ibuffer)

  ("m" . view-echo-area-messages)
  ("x" . exchange-point-and-mark)

  ("C-_" . ymacs-editor/goto-last-point)
  ("C-/" . ymacs-editor/goto-last-point)

  ("o" . ace-window)
  ("b" . ivy-switch-buffer)
  ("k" . kill-buffer)

  ("n n" . ymacs-editor/narrow-or-widen-dwim)
  (", ," . ymacs-editor-view-code-mode)

  ("{" . ymacs-hydra/window/shrink-window-horizontally)
  ("}" . ymacs-hydra/window/enlarge-window-horizontally)
  ("^" . ymacs-hydra/window/enlarge-window)
  ("-" . ymacs-hydra/window/shrink-window)
  ("?" . ymacs-hydra/window/body)
  ("SPC" . ymacs-hydra/rectangle/body)

  (", s" . ymacs-hydra/sort/body)
  (", e" . ymacs-hydra/ediff/body)
  ;; Minor mode to make xref use etags again.
  (", x" . xref-etags-mode)
  (", SPC" . ymacs-editor/insert-space-around-chinese))

(define-key!
  ("C-c O" . ymacs-hydra/outline/body)
  ("C-c C-b" . ymacs-editor/format-paragraph)

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

  ("C-'" . avy-goto-char-timer)

  ("M-0" . ymacs-editor/goto-char-or-minibuffer)
  ("M-7" . ymacs-editor/avy-copy-and-yank)
  ("M-8" . avy-goto-word-or-subword-1)
  ("M-9" . avy-goto-char)
  ("M-g w" . ymacs-editor/avy-copy)
  ("M-g y" . ymacs-editor/avy-copy-and-yank)
  ("M-g 1" . avy-goto-char)
  ("M-g 2" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g s" . avy-goto-symbol-1)
  ("M-g L" . avy-copy-line)

  ("M-s f" . ymacs-editor/font-faces-at-point)
  ("M-s o" . ymacs-editor/occur-dwim)

  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("RET" . newline-and-indent)

  ("M-/" . hippie-expand)

  ("M-n" . next-error)
  ("M-p" . previous-error)
  ("M-N" . ymacs-editor/select-error-buffer)

  ("M-'" . ymacs-editor/change-surround)

  ([remap move-beginning-of-line] . ymacs-editor/smart-move-begining-of-line)

  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)
  ([f6] . ymacs-editor/toggle-company-ispell)
  ([C-f7] . ymacs-editor/rsync-project)
  ([f7] . ymacs-hydra/toggles)
  ([f10] . ymacs-editor/compile))

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
  ("|" . mc/vertical-align)
  ("r" . mc/reverse-regions)
  ("." . ymacs-hydra/mc/mc/mark-next-like-this)
  ("," . ymacs-hydra/mc/mc/mark-previous-like-this)
  ([C-S-mouse-1] . mc/add-cursor-on-click))

(define-key! :prefix "C-c i"
  ("r" . ivy-resume)
  ("e" . counsel-compilation-errors)

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
  ("b" . counsel-bookmark)
  ("u" . counsel-unicode-char)
  ("d" . counsel-dired-jump)
  ("x" . counsel-linux-app)
  ("g" . counsel-git)
  ("G" . counsel-git-log)
  ("s" . counsel-git-grep)
  ("S" . counsel-git-stash)
  ("h" . counsel-minibuffer-history)
  ("m" . counsel-mark-ring)
  ("/" . counsel-grep)
  ("L" . counsel-locate)
  ("f" . counsel-faces)
  ("S" . counsel-find-symbol)
  ("F" . counsel-fonts)
  ("w" . counsel-colors-web)
  ("E" . counsel-colors-emacs)
  ("O" . counsel-outline)
  ("o" . counsel-org-goto-all)
  ("t" . counsel-tmm))

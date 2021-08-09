;;; -*- lexical-binding: t; -*-

(executable! gls)
(executable! ripgrep :exe "rg")
(executable! fdfind :exe ["fdfind" "fd"])
(executable! ctags :exe ["ctags-universal" "ctags"])

(option! default-project nil
  :type 'list
  :safe #'(lambda (x) (and (symbolp (car-safe x)) (stringp (cdr-safe x)))))

(eval-when-compile-config!
 (when (and ymacs-ripgrep-path
            (not (eq (shell-command
                      (concat ymacs-ripgrep-path " --pcre2-version"))
                     0)))
   (message "You need install ripgrep with pcre2 support (@see %s)"
            (expand-etc! "setup/install_rust.sh"))))

(require-packages!
 (exec-path-from-shell :when sys/macp)
 (fcitx :when sys/linuxp)
 company
 consult
 embark-consult
 selectrum
 orderless
 citre
 pretty-hydra
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
 sudo-edit
 which-key
 highlight-indentation)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

(autoload 'ymacs-editor/ripgrep (expand! "commands-completion.el") nil t)

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
(autoload 'ymacs-hydra/last-point/goto-last-change (expand! "commands-hydra") nil t)
(autoload 'ymacs-hydra/query-replace/body (expand! "commands-hydra") nil t)

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
  ("e" . scroll-up-line)
  ("u" . scroll-down-command)
  ("y" . scroll-down-line))

(define-key! :prefix "C-x"
  ("'")                                 ; unbind

  (", ," . ymacs-editor-view-code-mode)
  (", -" . ymacs-editor/copy-file-name)
  (", SPC" . ymacs-editor/insert-space-around-chinese)
  (", c" . ymacs-desktop/change-or-new-desktop)
  (", d" . ymacs-desktop/delete-desktop)
  (", e" . ymacs-hydra/ediff/body)
  (", s" . ymacs-hydra/sort/body)
  (", x" . xref-etags-mode) ;; Minor mode to make xref use etags again.

  ("2" . ymacs-editor/window-split-vertically)
  ("3" . ymacs-editor/window-split-horizontally)
  ("4 b" . consult-buffer-other-window)
  ("5 b" . consult-buffer-other-frame)

  ("-" . ymacs-hydra/window/shrink-window)
  ("?" . ymacs-hydra/window/body)

  ("C-/" . ymacs-hydra/last-point/goto-last-change)
  ("C-_" . ymacs-hydra/last-point/goto-last-change)

  ("D" . ymacs-editor/delete-this-file)
  ("G" . revert-buffer)
  ("I" . clone-indirect-buffer)
  ("R" . ymacs-editor/rename-this-file-and-buffer)
  ("W" . ymacs-editor/copy-this-file)

  ("SPC" . ymacs-hydra/rectangle/body)

  ("b" . consult-buffer)
  ("c" . ymacs-editor/cleanup-buffer-safe)
  ("k" . kill-buffer)
  ("m" . view-echo-area-messages)
  ("n n" . ymacs-editor/narrow-or-widen-dwim)
  ("o" . ace-window)

  ("w [" . winner-undo)
  ("w ]" . winner-redo)
  ("w w" . ymacs-editor/move-buffer)

  ("x" . exchange-point-and-mark)

  ("^" . ymacs-hydra/window/enlarge-window)
  ("_" . ymacs-editor/window-force-split-vertically)
  ("{" . ymacs-hydra/window/shrink-window-horizontally)
  ("|" . ymacs-editor/window-force-split-horizontally)
  ("}" . ymacs-hydra/window/enlarge-window-horizontally)

  (("C-b" "B") . ibuffer))

(define-key!
  ("<help> a" . consult-apropos)

  ("C-'" . avy-goto-char-timer)

  ("C--" . ymacs-hydra/mc/mc/mark-previous-like-this)
  ("C-<" . ymacs-hydra/mc/mc/mark-previous-like-this)
  ("C-=" . ymacs-hydra/mc/mc/mark-next-like-this)
  ("C->" . ymacs-hydra/mc/mc/mark-next-like-this)

  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-c C-b" . ymacs-editor/format-paragraph)
  ("C-c F" . company-files)
  ("C-}" . company-yasnippet)

  ("C-M-b" . backward-sentence)
  ("C-M-f" . forward-sentence)
  ("C-M-k" . kill-sentence)

  ("C-r" . consult-line-multi)
  ("C-s" . consult-line)

  ("M-%" . ymacs-hydra/query-replace/body)
  ("M-'" . ymacs-editor/change-surround)
  ("M-*" . citre-jump)
  ("M--" . er/expand-region)
  ("M-/" . hippie-expand)

  ("M-0" . ymacs-editor/goto-char-or-minibuffer)
  ("M-7" . ymacs-editor/avy-copy-and-yank)
  ("M-8" . embark-act)
  ("M-9" . avy-goto-word-or-subword-1)
  ("M-;" . ymacs-editor/comment-dwim)
  ("M-I" . iedit-rectangle-mode)
  ("M-N" . ymacs-editor/select-error-buffer)
  ("M-a" . ymacs-editor/backward-sexp)
  ("M-e" . ymacs-editor/forward-sexp)
  ("M-i" . ymacs-editor/iedit-mode)
  ("M-k" . kill-sexp)
  ("M-n" . next-error)
  ("M-p" . previous-error)
  ("M-y" . consult-yank-pop)
  ("M-{" . ymacs-editor/backward-defun)
  ("M-}" . ymacs-editor/forward-defun)

  ("RET" . newline-and-indent)

  ([C-f7] . ymacs-editor/rsync-project)
  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)
  ([f10] . ymacs-editor/compile)
  ([f6] . ymacs-editor/toggle-company-ispell)
  ([f7] . ymacs-hydra/toggles)

  ([remap move-beginning-of-line] . ymacs-editor/smart-move-begining-of-line))

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

(define-key! :prefix "M-g"
  ("O" . ymacs-hydra/outline/body)
  ("e" . consult-compile-error)
  ("f" . consult-flymake)
  ("g" . consult-goto-line)  ;; orig. goto-line
  ("M-g" . consult-goto-line) ;; orig. goto-line
  ("o" . consult-outline)     ;; Alternative: consult-org-heading
  ("m" . consult-mark)
  ("k" . consult-global-mark)
  ("i" . consult-imenu)
  ("I" . consult-imenu-multi)
  ("." . citre-ace-peek)
  ("u" . browse-url-at-point)
  ("w" . ymacs-editor/avy-copy)
  ("y" . ymacs-editor/avy-copy-and-yank)
  ("1" . avy-goto-char)
  ("2" . avy-goto-char-2)
  ("l" . avy-goto-line)
  ("s" . avy-goto-symbol-1)
  ("L" . avy-copy-line))

(define-key! :prefix "C-c i"
  ("e" . sudo-edit)

  ("l l" . load-library)
  ("l t" . consult-theme)
  ("l p" . list-processes)
  ("l f" . find-library)

  ("j" . consult-find)
  ("a" . ymacs-editor/ripgrep)
  ("b" . consult-bookmark)
  ("i" . consult-imenu)
  ("L" . consult-locate)
  ("k" . consult-keep-lines)
  ("g" . consult-git-grep)
  ("/" . consult-grep)
  ("f" . customize-face)
  ("o" . consult-outline))

(define-key! :prefix "M-s"
  ("e" . consult-isearch)
  ("f" . ymacs-editor/font-faces-at-point)
  ("k" . consult-keep-lines)
  ("o" . ymacs-editor/occur-dwim)
  ("u" . consult-focus-lines))

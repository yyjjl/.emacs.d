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
 corfu
 corfu-terminal
 cape
 consult
 embark-consult
 vertico
 orderless
 marginalia
 yasnippet
 expand-region
 goto-chg
 multiple-cursors
 iedit
 wgrep
 ace-window
 persistent-scratch
 hl-todo
 page-break-lines
 sudo-edit
 which-key
 lv
 highlight-indentation)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

(autoload 'ymacs-editor/ripgrep (expand! "commands-rg") nil t)

(autoload 'ymacs-transient/ediff (expand! "commands-transient") nil t)
(autoload 'ymacs-transient/outline (expand! "commands-transient") nil t)
(autoload 'ymacs-transient/window (expand! "commands-transient") nil t)
(autoload 'ymacs-transient/sort (expand! "commands-transient") nil t)
(autoload 'ymacs-transient/query-replace (expand! "commands-transient") nil t)

(autoload 'xref-pulse-momentarily "xref")
(autoload 'project-root "project")
(autoload 'project-current "project")
(autoload 'ansi-color-apply-on-region "ansi-color")
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
  (", e" . ymacs-transient/ediff)
  (", s" . ymacs-transient/sort)
  (", x" . xref-etags-mode) ;; Minor mode to make xref use etags again.

  ("2" . ymacs-editor/window-split-vertically)
  ("3" . ymacs-editor/window-split-horizontally)

  ("?" . ymacs-transient/window)

  ("C-/" . goto-last-change)
  ("C-_" . goto-last-change)

  ("D" . ymacs-editor/delete-this-file)
  ("G" . revert-buffer)
  ("I" . clone-indirect-buffer)
  ("R" . ymacs-editor/rename-this-file-and-buffer)
  ("W" . ymacs-editor/copy-this-file)

  ("SPC" . rectangle-mark-mode)

  ("c" . ymacs-editor/cleanup-buffer-safe)
  ("k" . kill-buffer)
  ("m" . view-echo-area-messages)
  ("n n" . ymacs-editor/narrow-or-widen-dwim)
  ("o" . ace-window)

  ("w [" . winner-undo)
  ("w ]" . winner-redo)
  ("w w" . ymacs-editor/move-buffer)

  ("j" . ymacs-editor/find-file)

  (("C-b" "B") . ibuffer))

(define-key!
  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-." . embark-act)
  ("C-;" . embark-dwan)

  ("C-c C-b" . ymacs-editor/format-paragraph)
  ("C-c F" . cape-file)

  ("C-k" . ymacs-editor/smart-kill-line)

  ("C-r" . consult-line-multi)
  ("C-s" . ymacs-editor/consult-ripgrep-or-line)

  ("M-%" . ymacs-transient/query-replace)
  ("M-'" . ymacs-editor/change-surround)
  ("M--" . er/expand-region)
  ("M-/" . hippie-expand)

  ("M-0" . ymacs-editor/goto-char-or-minibuffer)
  ("M-8" . ymacs-editor/avy-copy-and-yank)
  ("M-9" . avy-goto-char-timer)
  ("M-;" . ymacs-editor/comment-dwim)
  ("M-I" . iedit-rectangle-mode)
  ("M-N" . ymacs-editor/select-error-buffer)
  ("M-a" . ymacs-editor/backward-sexp)
  ("M-e" . ymacs-editor/forward-sexp)
  ("M-h" . ymacs-editor/smart-M-h)
  ("M-k" . ymacs-editor/ripgrep)
  ("M-i" . ymacs-editor/iedit-mode)
  ("M-n" . next-error)
  ("M-p" . previous-error)
  ("M-{" . ymacs-editor/backward-defun)
  ("M-}" . ymacs-editor/forward-defun)

  ("RET" . newline-and-indent)

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
  ([C-S-mouse-1] . mc/add-cursor-on-click))

(define-key! :prefix "M-g"
  ("O" . ymacs-transient/outline)
  ("b" . bookmark-jump)
  ("e" . consult-compile-error)
  ("f" . consult-flymake)
  ("o" . consult-outline)
  ("m" . consult-mark)
  ("k" . consult-global-mark)
  ("i" . consult-imenu-multi)
  ("u" . browse-url-at-point)
  ("w" . ymacs-editor/avy-copy)
  ("y" . ymacs-editor/avy-copy-and-yank)
  ("1" . avy-goto-char)
  ("2" . avy-goto-char-2)
  ("l" . avy-goto-line)
  ("s" . avy-goto-symbol-1)
  ("L" . locate))

(define-key! :prefix "C-c i"
  ("e" . sudo-edit)
  ("a" . ymacs-editor/ripgrep)

  ("l t" . load-theme)
  ("l l" . load-library)
  ("l p" . list-processes)
  ("l f" . find-library)

  ("i" . consult-imenu-multi)
  ("m" . man)
  ("g" . consult-git-grep)
  ("/" . consult-grep)
  ("f" . customize-face))

(define-key! :prefix "M-s"
  ("e" . consult-isearch-history)
  ("f" . ymacs-editor/font-faces-at-point)
  ("k" . consult-keep-lines)
  ("o" . ymacs-editor/occur-dwim)
  ("u" . consult-focus-lines))

(define-key!
  ([remap move-beginning-of-line] . ymacs-editor/smart-move-begining-of-line)
  ([remap apropos] . consult-apropos)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop))

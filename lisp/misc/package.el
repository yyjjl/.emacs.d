;; -*- lexical-binding: t -*-

(executable! fcitx)

(require-packages!
 (fcitx :when ymacs-fcitx-path)
 zeal-at-point
 buffer-move
 whitespace
 csv-mode
 gnuplot-mode
 graphviz-dot-mode
 yaml-mode)

(defcustom ymacs-misc-project-rsync-remote-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-misc-project-rsync-local-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'file-directory-p)

(defcustom ymacs-misc-project-rsync-extra-options nil
  "."
  :group 'projectile
  :type 'directory
  :safe (lambda (x) (and (listp x) (-all? #'stringp x))))

(defcustom ymacs-misc-local-snippets-list nil
  "local snippets"
  :group 'editing
  :type 'directory
  :safe #'listp)

(defvar ymacs-misc-after-rename-this-file-hook nil)
(defvar ymacs-misc-after-delete-this-file-hook nil)
(defvar ymacs-misc-after-search-hook nil)

(defvar ymacs-misc-view-code-modes
  '((t display-line-numbers-mode
       view-mode
       highlight-indentation-current-column-mode
       highlight-indentation-mode)))

(defvar ymacs-savehist-exclude-variables
  '(load-history
    register-alist
    vc-comment-ring
    flyspell-auto-correct-ring
    org-mark-ring
    planner-browser-file-display-rule-ring))

(defvar ymacs-misc-project-rsync-command
  "rsync -azh --progress --filter=':- .gitignore' %s %s %s")

(defvar ymacs-misc-search-engine-alist
  '(("g" "google" "http://www.google.com/search?q=%s")
    ("q" "stackoverflow" "http://www.google.com/search?q=%s+site:stackoverflow.com")
    ("w" "wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
    ("d" "dictionary" "http://dictionary.reference.com/search?q=%s")
    ("cpp" "cpp" "https://www.google.com/search?q=cpp+%s")))

(defvar ymacs-misc-socks-server '("Default server" "127.0.0.1" 1080 5))

;; Smart tab
(defvar ymacs-misc--indent-close-list '(?\} ?\$ ?\] ?\' ?\` ?\"))
(defvar ymacs-misc--indent-compelte-functions
  '(ymacs-misc//try-expand-local-snippets
    company-complete
    hippie-expand))

(defvar ymacs-misc-projectile-invalidate-cache-empty-vars
  '(doom-modeline--vcs-text
    doom-modeline--project-detected-p
    doom-modeline--project-root
    elpy-project-root))

(defvar ymacs-misc-auto-next-error-buffer-derived-modes
  '(occur-mode
    grep-mode
    ivy-occur-mode
    xref--xref-buffer-mode
    compilation-mode))

(define-key! :prefix "C-x"
  ("2" . ymacs-window/split-vertically)
  ("3" . ymacs-window/split-horizontally)
  ("|" . ymacs-window/force-split-horizontally)
  ("_" . ymacs-window/force-split-vertically)
  ("?" . ymacs-window/split-window-to-grid)

  (", ," . ymacs-misc/view-code-mode)
  (", a" . ymacs-misc/add-local-snippet)
  (", g" . ymacs-misc/search-in-chrome)
  (", -" . ymacs-misc/copy-file-name)
  (", o" . recentf-open-files)
  ("C-b" . ibuffer)
  ("C-d" . find-name-dired)

  ("D" . ymacs-misc/delete-this-file)
  ("R" . ymacs-misc/rename-this-file-and-buffer)
  ("W" . ymacs-misc/copy-this-file-to-new-file)
  ("c" . ymacs-misc/cleanup-buffer-safe)
  ("o" . ace-window)
  ("m" . view-echo-area-messages)

  ("w [" . winner-undo)
  ("w ]" . winner-redo))

(define-key!
  ("C-x C-_" . goto-last-change)
  ("C-x C-/" . goto-last-change)

  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-x G" . revert-buffer)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)

  ("M-/" . hippie-expand)
  ("M-n" . next-error)
  ("M-N" . ymacs-misc/select-next-error-buffer)
  ("M-p" . previous-error)

  ("M-`" . other-frame)
  ("M-i" . iedit-mode)
  ("M-s e" . ymacs-misc/eval-and-replace)
  ("M-s o" . ymacs-misc/occur-dwim)
  ("RET" . newline-and-indent)

  ([C-f7] . ymacs-misc/rsync-project)

  ([f10] . compile))

(define-key! :map indent-rigidly-map
  ("[" . indent-rigidly-left)
  ("]" . indent-rigidly-right)
  ("{" . indent-rigidly-left-to-tab-stop)
  ("}" . indent-rigidly-right-to-tab-stop))

(define-key! :map special-mode-map
  ("u" . scroll-down-command)
  ("y" . scroll-down-line)
  ("e" . scroll-up-line))

(define-key! :prefix "C-x w"
  ;; buffer-mode
  ("k" . buf-move-up)
  ("j" . buf-move-down)
  ("h" . buf-move-left)
  ("l" . buf-move-right))

(define-key! :prefix "C-h"
  ("z" . zeal-at-point)
  ("Z" . zeal-at-point-search))

(autoload 'sdcv-current-word "sdcv" nil t)
(autoload 'sdcv-goto-sdcv "sdcv" nil t)

(add-auto-mode! 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)

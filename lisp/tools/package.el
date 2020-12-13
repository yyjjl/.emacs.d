;; -*- lexical-binding: t -*-

(executable! fcitx)

(require-packages!
 (exec-path-from-shell :when sys/macp)
 (fcitx :when ymacs-fcitx-path)
 bison-mode
 buffer-move
 csv-mode
 gnuplot-mode
 crontab-mode
 graphviz-dot-mode
 projectile
 whitespace
 yaml-mode
 yasnippet
 yasnippet-snippets
 zeal-at-point)

(defcustom ymacs-tools-project-rsync-remote-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-tools-project-rsync-local-path nil
  "."
  :group 'projectile
  :type 'directory
  :safe #'file-directory-p)

(defcustom ymacs-tools-project-rsync-extra-options nil
  "."
  :group 'projectile
  :type 'directory
  :safe (lambda (x) (and (listp x) (-all? #'stringp x))))

(defcustom ymacs-tools-local-snippets-list nil
  "local snippets"
  :group 'editing
  :type 'directory
  :safe #'listp)

(defvar ymacs-tools-after-rename-this-file-hook nil)
(defvar ymacs-tools-after-delete-this-file-hook nil)
(defvar ymacs-tools-after-search-hook nil)

(defvar ymacs-savehist-exclude-variables
  '(load-history
    register-alist
    vc-comment-ring
    flyspell-auto-correct-ring
    org-mark-ring
    planner-browser-file-display-rule-ring))

(defvar ymacs-tools-project-rsync-command
  "rsync -azh --progress --filter=':- .gitignore' %s %s %s")

(defvar ymacs-tools-search-engine-alist
  '(("g" "google" "http://www.google.com/search?q=%s")
    ("q" "stackoverflow" "http://www.google.com/search?q=%s+site:stackoverflow.com")
    ("w" "wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s")
    ("d" "dictionary" "http://dictionary.reference.com/search?q=%s")
    ("cpp" "cpp" "https://www.google.com/search?q=cpp+%s")))

(defvar ymacs-tools-socks-server '("Default server" "127.0.0.1" 1080 5))

(defvar ymacs-tools-projectile-invalidate-cache-empty-vars
  '(doom-modeline--vcs-text
    doom-modeline--project-detected-p
    doom-modeline--project-root))

(declare-function winner-undo 'winner)
(declare-function winner-redo 'winner)

(define-key! :prefix "C-x"
  ("2" . ymacs-window/split-vertically)
  ("3" . ymacs-window/split-horizontally)
  ("|" . ymacs-window/force-split-horizontally)
  ("_" . ymacs-window/force-split-vertically)

  (", a" . ymacs-tools/add-local-snippet)
  (", g" . ymacs-tools/search-in-chrome)
  (", -" . ymacs-tools/copy-file-name)
  (", o" . recentf-open-files)
  ("C-b" . ibuffer)
  ("C-d" . find-name-dired)

  ("D" . ymacs-tools/delete-this-file)
  ("R" . ymacs-tools/rename-this-file-and-buffer)
  ("W" . ymacs-tools/copy-this-file-to-new-file)
  ("c" . ymacs-tools/cleanup-buffer-safe)
  ("o" . ace-window)
  ("m" . view-echo-area-messages)

  ("w [" . winner-undo)
  ("w ]" . winner-redo))

(define-key!
  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-x G" . revert-buffer)

  ("C-c 4" . ispell-word)
  ("C-c q" . auto-fill-mode)

  ("M-/" . hippie-expand)
  ("M-n" . next-error)
  ("M-N" . ymacs-tools/select-next-error-buffer)
  ("M-p" . previous-error)

  ("M-`" . other-frame)
  ("M-i" . iedit-mode)
  ("M-s e" . ymacs-tools/eval-and-replace)
  ("M-s o" . ymacs-tools/occur-dwim)
  ("RET" . newline-and-indent)

  ([C-f7] . ymacs-tools/rsync-project)

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

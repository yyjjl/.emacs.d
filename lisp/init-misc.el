(autoload 'glsl-mode "glsl-mode" "Major mode for OpenGL Shader langugae" t)
;; popwin setup
(autoload 'popwin-mode "popwin" nil t)

;; input open source license
(autoload 'legalese "legalese" "" t)

(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap))
;; turns on auto-fill-mode, don't use text-mode-hook
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)

;;----------------------------------------------------------------------------

;; {{ buf-move
(bind-keys ("C-c w i" . buf-move-up)
           ("C-c w k" . buf-move-down)
           ("C-c w j" . buf-move-left)
           ("C-c w l" . buf-move-right))
;; }}

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

;; {{ recentf-mode
(transient-mark-mode t)
(recentf-mode 1)

(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"))
;; }}

;; https://github.com/abo-abo/ace-window
;; `M-x ace-window ENTER m` to swap window
(global-set-key (kbd "C-x o") 'ace-window)

;; {{ avy, jump between texts, like easymotion in vim
;; emacs key binding, copied from avy website
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
;; }}

;; automatic save place of each buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/data/places")

;; ANSI-escape coloring in compilation-mode
;; {{ http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
;; }}

;; {{ tramp setup
;; @see http://www.quora.com/Whats-the-best-way-to-edit-remote-files-from-Emacs
(setq tramp-default-method "ssh")
(setq trampv-auto-save-directory "~/.emacs.d/data/tramp/")
(setq tramp-backup-directory-alist '(("." . "~/.emacs.d/data/tramp/")))
(setq tramp-chunksize 8192)
;; @see https://github.com/syl20bnr/spacemacs/issues/1921
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;; }}

;; {{ minibuffer-hook
(defun my-minibuffer-setup-hook ()
  ;; Use paredit in the minibuffer
  (conditionally-paredit-mode 1)
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  (local-set-key (kbd "C-k") 'kill-line)
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (conditionally-paredit-mode -1)
  (setq gc-cons-threshold (* 100 1024 1024)))

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
;; }}

;; make scratch buffer unkillable
(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)



;; I'm in Australia now, so I set the locale to "en_AU"
(defun insert-date (prefix)
  "Insert the current date.
With prefix-argument, use ISO format.  With
two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d %B %Y"))))
    (insert (format-time-string format))))

;;compute the length of the marked region
(defun region-length ()
  "Length of a region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))


;; {{ imenu tweak
(defvar rimenu-position-pair nil "Positions before and after imenu jump.")
(add-hook 'imenu-after-jump-hook
          (lambda ()
            (let ((start-point (marker-position (car mark-ring)))
                  (end-point (point)))
              (setq rimenu-position-pair (list start-point end-point)))))

(defun rimenu-jump ()
  "Jump to the closest before/after position of latest imenu jump."
  (interactive)
  (when rimenu-position-pair
    (let ((p1 (car rimenu-position-pair))
          (p2 (cadr rimenu-position-pair)))

      ;; jump to the far way point of the rimenu-position-pair
      (if (< (abs (- (point) p1))
             (abs (- (point) p2)))
          (goto-char p2)
        (goto-char p1))
      )))
;; }}

;; show ascii table
(defun ascii-table ()
  "Print the ascii table.  Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

;; {{ grep and kill-ring
(defun grep-pattern-into-list (regexp)
  (let ((s (buffer-string))
        (pos 0)
        item
        items)
    (while (setq pos (string-match regexp s pos))
      (setq item (match-string-no-properties 0 s))
      (setq pos (+ pos (length item)))
      (if (not (member item items))
          (add-to-list 'items item)
        ))
    items))

(defun grep-pattern-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string and insert them into `kill-ring'"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "%s\n" i)))
      )
    (kill-new rlt)
    (message "matched strings => kill-ring")
    rlt))

(defun grep-pattern-jsonize-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string, jsonize them, and insert into kill ring"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq rlt (concat rlt (format "%s : %s ,\n" i i)))
      )
    (kill-new rlt)
    (message "matched strings => json => kill-ring")
    rlt))

(defun grep-pattern-cssize-into-kill-ring (regexp)
  "Find all strings matching REGEXP in current buffer.
grab matched string, cssize them, and insert into kill ring"
  (interactive
   (let* ((regexp (read-regexp "grep regex:")))
     (list regexp)))
  (let (items rlt)
    (setq items (grep-pattern-into-list regexp))
    (dolist (i items)
      (setq i (replace-regexp-in-string "\\(class=\\|\"\\)" "" i))
      (setq rlt (concat rlt (format ".%s {\n}\n\n" i))))
    (kill-new rlt)
    (message "matched strings => json => kill-ring")
    rlt))
;; }}


(with-eval-after-load 'grep
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               ".sass_cache"
               ".cache"
               ".git"
               ".cvs"
               ".svn"
               ".hg"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v)))

;; {{ unique lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
;; }}


(defun current-font-face ()
  "Get the font face under cursor."
  (interactive)
  (let ((rlt (format "%S" (get-text-property (point) 'face))))
    (kill-new rlt)
    (copy-yank-str rlt)
    (message "%s => clipboard & yank ring" rlt)))

(defun toggle-env-http-proxy ()
  "Set/unset the environment variable http_proxy which w3m uses."
  (interactive)
  (let ((proxy "http://127.0.0.1:1080"))
    (if (string= (getenv "http_proxy") proxy)
        ;; clear the the proxy
        (progn
          (setenv "http_proxy" "")
          (message "env http_proxy is empty now")
          )
      ;; set the proxy
      (setenv "http_proxy" proxy)
      (message "env http_proxy is %s now" proxy))))

;; {{ save history
;; On Corp machines, I don't have permission to access history,
;; so safe-wrap is used
(safe-wrap
 (if (file-writable-p (file-truename "~/.emacs.d/data/history"))
     (progn
       (setq history-length 8000)
       (setq savehist-additional-variables
             '(search-ring regexp-search-ring kill-ring))
       (savehist-mode 1))
   (message "Failed to access ~/.emacs.d/data/history")))
;; }}

;; ------------------------------------------------------------


(global-set-key (kbd "C-h z") 'zeal-at-point)


;; {{start dictionary lookup
;; use below commands to create dicitonary
;; mkdir -p ~/.stardict/dic
;; # wordnet English => English
;; curl http://abloz.com/huzheng/stardict-dic/dict.org/stardict-dictd_www.dict.org_wn-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;; # Langdao Chinese => English
;; curl http://abloz.com/huzheng/stardict-dic/zh_CN/stardict-langdao-ec-gb-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;;
(autoload 'sdcv-search "sdcv" nil t)
(global-set-key (kbd "C-c D") 'sdcv-search)
;; }}

(global-set-key (kbd "C-x j j") 'bookmark-jump)
(setq bmkp-bookmark-map-prefix-keys nil)

;; midnight mode purges buffers which haven't been displayed in 3 days
(require 'midnight)
(setq midnight-mode t)

(provide 'init-misc)

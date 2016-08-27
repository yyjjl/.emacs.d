;; popwin setup
(with-eval-after-load 'popwin
  (global-set-key (kbd "C-z") popwin:keymap)
  (push '(direx:direx-mode :position left :dedicated t)
        popwin:special-display-config)

  (define-key popwin:keymap "d"
    'direx:jump-to-directory-other-window)
  (define-key popwin:keymap "r"
    'direx-project:jump-to-project-root-other-window))

;; {{ guide-key-mode
(with-eval-after-load 'guide-key
  (setq guide-key/guide-key-sequence
        '("C-x v" ; VCS commands
          "C-c" "C-z" "C-x r"
          "C-x g"
          "C-x 4"
          "C-x 5"
          (dired-mode "]" "[" "%")))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/idle-delay 0.5))
;; }}

;; turns on auto-fill-mode, don't use text-mode-hook because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;----------------------------------------------------------------------------

;; input open source license
(autoload 'legalese "legalese" "" t)

;; {{ buf-move
(bind-keys ("C-c b i" . buf-move-up)
           ("C-c b k" . buf-move-down)
           ("C-c b j" . buf-move-left)
           ("C-c b l" . buf-move-right))
;; }}

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

;; {{ recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"))
;; }}

;; @see http://www.emacswiki.org/emacs/EasyPG#toc4
;; besides, use gnupg 1.4.9 instead of 2.0
(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

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
(setq tramp-auto-save-directory "~/.emacs.d/data/tramp/")
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

(provide 'init-misc)

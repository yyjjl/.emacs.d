;; prolog system
(setq prolog-system 'swi)
;; pulse
(setq pulse-delay 0.05)
;; csv
(setq csv-separators '("," ";" "|" " "))

(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'grep
  (dolist (v core|ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))

;; Zeal at point
(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python"))

;; Star dictionary lookup
(autoload 'sdcv-search "sdcv" nil t)
(global-set-key (kbd "C-c D") 'sdcv-search)

;; `restclient-mode'
(with-eval-after-load 'restclient
  (define-keys :map restclient-mode-map
    ("M-p" . restclient-jump-prev)
    ("M-n" . restclient-jump-next)))
(defhook extra|restclient-setup (restclient-mode-hook)
  (add-to-list 'company-backends 'company-restclient))

(defconst extra|ascii-before-chinese
  (rx (group-n 1 (in "a-zA-Z0-9!@#$%^&\\-+|)\\]}\\:;?><.,"))
      (group-n 2 (category chinese-two-byte))))
(defconst extra|ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in "a-zA-Z0-9@#$%^&\\-+|(\\[{\\></"))))

(defun extra|insert-space-around-chinese (&optional start end)
  (interactive)
  (if (region-active-p)
      (setq start (region-beginning)
            end (region-end))
    (setq start (point-min)
          end (point-max)))
  (save-excursion
    (goto-char start)
    (while (re-search-forward extra|ascii-before-chinese end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char start)
    (while (re-search-forward extra|ascii-after-chinese end t)
      (replace-match "\\1 \\2" nil nil))))

(global-set-key (kbd "M-Q") 'extra|insert-space-around-chinese)

(defun extra|clipboard-copy (beg end)
  (interactive "r")
  (if (display-graphic-p)
      (kill-new (buffer-substring-no-properties beg end))
    (if emacs|has-xsel-p
        (if (= 0 (shell-command-on-region beg end "xsel -ib"))
            (message "Copy finished")
          (message "Error occured !!!"))
      (message "Executable `xsel' not found !!!"))))

(defun extra|clipboard-paste ()
  (interactive)
  (if (display-graphic-p)
      (yank 1)
    (if emacs|has-xsel-p
        (shell-command "xsel -ob" t)
      (message "Executable `xsel' not found !!!"))))

;; `whitespace-space' setup
(with-eval-after-load 'whitespace
  (setq whitespace-style '(face spaces tabs newline
                                space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [182 10]))
  (setcdr (assoc 'tab-mark whitespace-display-mappings)
          '(9 [8594] [92 9])))

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

;; `doxygen' setup
(autoload 'doxygen-insert-function-comment "doxygen" "insert comment for the function at point" t)
(autoload 'doxygen-insert-file-comment "doxygen" "insert comment for file" t)
(autoload 'doxygen-insert-member-group-region "doxygen" "insert comment for member group" t)
(autoload 'doxygen-insert-compound-comment "doxygen" "insert comment for compound" t)

(with-eval-after-load 'emms
  (emms-all)
  (emms-mode-line 0)
  (emms-playing-time-disable-display)
  ;; (setq emms-playing-time-display-format " (%s) ")
  (setq emms-mode-line-format "%s")
  (setq emms-source-file-default-directory "~/music/")
  (setq emms-player-list '(emms-player-mplayer))

  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode)

  (autoload 'epe-fish-path "eshell-prompt-extras"))

(when emacs|has-mpv-p
  (require 'emms)
  (defun extra|emms-current-name ()
    (concat (propertize
             (epe-fish-path (or (emms-mode-line-playlist-current)
                                ""))
             'face 'font-lock-constant-face)))
  (defhydra hydra|emms (:color pink :hint nil)
    "
%s(extra|emms-current-name)
[_=_/_-_]volume raise/lower  [_d_/_D_]play directory subtree/current
[_p_]rev [_n_]ext            [_t_/_T_]oggle repeat playlist/track
[_s_]top [_P_]ause/Continue  [_r_]andom
[_SPC_]Select playlist     [_RET_]Open playlist buffer                [_q_]uit
"
    ("n" emms-next)
    ("p" emms-previous)
    ("s" emms-stop :exit t)
    ("P" emms-pause)
    ("r" emms-random)
    ("d" emms-play-directory-tree)
    ("D" emms-play-directory)
    ("SPC" emms-play-playlist)
    ("RET" emms-playlist-mode-go :exit t)
    ("t" emms-toggle-repeat-playlist)
    ("T" emms-toggle-repeat-track)
    ("-" emms-volume-lower)
    ("=" emms-volume-raise)
    ("q" nil nil))
  (global-set-key [f12] #'hydra|emms/body))

(define-keys
  ;; buffer-mode
  ("C-c w k" . buf-move-up)
  ("C-c w j" . buf-move-down)
  ("C-c w h" . buf-move-left)
  ("C-c w l" . buf-move-right)
  ;; `ace-window' and `avy'
  ("C-x o" . ace-window)
  ("M-g 1" . avy-goto-char)
  ("M-g 2" . avy-goto-char-2)
  ("M-g l" . avy-goto-line)
  ("M-g s" . avy-goto-symbol-1)
  ("M-g w" . avy-goto-word-1)
  ("M-g y" . avy-copy-line))

(provide 'init-extra)

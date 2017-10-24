(require-packages!
 restclient
 company-restclient
 (emms :when emacs-has-mpv-p)
 (emms-player-mpv :when emacs-has-mpv-p)
 markdown-mode
 csv-mode
 glsl-mode
 php-mode
 gnuplot-mode
 csharp-mode
 graphviz-dot-mode
 ;; Move buffers between windows
 buffer-move
 figlet
 zeal-at-point
 skeletor
 slime)



;; prolog system
(setq prolog-system 'swi)
;; pulse
(setq pulse-delay 0.01)
;; csv
(setq csv-separators '(", " ";" "|" " "))

(defalias 'perl-mode 'cperl-mode)

(setq inferior-lisp-program "ccl")

(with-eval-after-load 'slime
  (setq slime-contribs '(slime-fancy))
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-complete-symbol*-fancy t))

(with-eval-after-load 'graphviz-dot-mode
  (defun extra/dot-complete ()
    (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
           (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
           (graphviz-dot-str (buffer-substring b e))
           (allcomp (all-completions graphviz-dot-str
                                     (graphviz-dot-get-keywords))))
      (list b e allcomp)))

  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4)

  (define-hook! extra|setup-dot (graphviz-dot-mode-hook)
    (add-to-list 'completion-at-point-functions 'extra/dot-complete)))

(with-eval-after-load 'grep
  (dolist (v core-ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))

;; Zeal at point
(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))

;; Star dictionary lookup
(autoload 'sdcv-current-word "sdcv" nil t)
(autoload 'sdcv-goto-sdcv "sdcv" nil t)

;; `restclient-mode'
(with-eval-after-load 'restclient
  (define-key! :map restclient-mode-map
    ("M-p" . restclient-jump-prev)
    ("M-n" . restclient-jump-next)))
(define-hook! extra|restclient-setup (restclient-mode-hook)
  (add-to-list 'company-backends 'company-restclient))

(defun extra/clipboard-copy ($beg $end)
  (interactive "r")
  (if (display-graphic-p)
      (kill-new (buffer-substring-no-properties $beg $end))
    (if emacs-has-xsel-p
        (if (= 0 (shell-command-on-region $beg $end "xsel -ib"))
            (message "Copy finished")
          (message "Error occured !!!"))
      (message "Executable `xsel' not found !!!"))))

(defun extra/clipboard-paste ()
  (interactive)
  (if (display-graphic-p)
      (yank 1)
    (if emacs-has-xsel-p
        (shell-command "xsel -ob" t)
      (message "Executable `xsel' not found !!!"))))

;; `whitespace-space' setup
(with-eval-after-load 'whitespace
  (setq whitespace-style '(face spaces tabs newline
                                space-mark tab-mark newline-mark))
  (setcdr (assoc 'newline-mark whitespace-display-mappings)
          '(10 [182 10]))
  (setcdr (assoc 'tab-mark whitespace-display-mappings)
          '(9 [8594 9])))

(with-eval-after-load 'skeletor
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir) (skeletor-async-shell-command "mkdir build"))))
(global-set-key (kbd "C-c p n") 'skeletor-create-project)
(global-set-key (kbd "C-c p N") 'skeletor-create-project-at)

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(with-eval-after-load 'emms
  (emms-all)
  (emms-mode-line -1)
  (emms-playing-time-disable-display)
  (emms-lyrics-disable)

  (setq emms-mode-line-format "%s")
  (setq emms-lyrics-display-p nil)
  (setq emms-source-file-default-directory "~/music/")
  (defvar emms-default-playlist
    (expand-file-name "all.pls" emms-source-file-default-directory))
  (setq emms-player-list '(emms-player-vlc))

  (advice-add 'emms-lyrics-display-handler :around #'core/ignore-error)
  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode))
(with-eval-after-load 'emms
  (require 'emms-player-mpv)
  (emms-all)
  (emms-mode-line -1)
  (emms-playing-time-disable-display)
  (emms-lyrics-disable)

  (setq emms-mode-line-format "%s")
  (setq emms-lyrics-display-p nil)
  (setq emms-source-file-default-directory "~/music/")
  (defvar emms-default-playlist
    (expand-file-name "all.pls" emms-source-file-default-directory))
  (setq emms-player-list '(emms-player-mpv))
  (add-to-list 'emms-player-mpv-parameters "--no-video")

  (advice-add 'emms-lyrics-display-handler :around #'core/ignore-error)
  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode))

(when emacs-has-mpv-p
  (defun extra/emms-current-name ()
    (concat (propertize
             (abbreviate-file-name (emms-mode-line-playlist-current))
             'face 'font-lock-constant-face)))
  (defhydra hydra-emms
    ( ;; options
     :color pink
     :hint nil
     :body-pre
     (unless (and (fboundp 'emms-playlist-current-selected-track)
                  (emms-playlist-current-selected-track))
       (unless (featurep 'emms) (require 'emms))
       (emms-play-playlist emms-default-playlist)))
    "
%s(extra/emms-current-name)
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
  (global-set-key [f12] #'hydra-emms/body))

(define-key!
  ;; buffer-mode
  ("C-x w k" . buf-move-up)
  ("C-x w j" . buf-move-down)
  ("C-x w h" . buf-move-left)
  ("C-x w l" . buf-move-right))

(provide 'init-extra)

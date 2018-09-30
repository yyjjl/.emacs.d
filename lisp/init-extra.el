;;; -*- lexical-binding: t; -*-

(require-packages!
 calc
 whitespace
 markdown-mode
 csv-mode
 php-mode
 gnuplot-mode
 csharp-mode
 graphviz-dot-mode
 yaml-mode
 ;; Move buffers between windows
 buffer-move
 zeal-at-point
 skeletor
 lua-mode
 restclient
 sly)

(add-auto-mode! 'crontab-mode "\\.?cron\\(tab\\)?\\'")
(add-auto-mode! 'markdown-mode "\\.\\(md\\|markdown\\)\\'")



;; Star dictionary lookup
(autoload 'sdcv-current-word "sdcv" nil t)
(autoload 'sdcv-goto-sdcv "sdcv" nil t)

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

;; prolog system
(setq prolog-system 'swi)
;; erc
(setq erc-track-enable-keybindings nil)
;; pulse
(setq pulse-delay 0.01)
;; csv
(setq csv-separators '("," ";" "|" " "))

(setq inferior-lisp-program "sbcl")

(define-key! :prefix "C-x w"
  ;; buffer-mode
  ("k" . buf-move-up)
  ("j" . buf-move-down)
  ("h" . buf-move-left)
  ("l" . buf-move-right))



(defun extra//dot-complete ()
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
         (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
         (graphviz-dot-str (buffer-substring b e))
         (allcomp (all-completions graphviz-dot-str
                                   (graphviz-dot-get-keywords))))
    (list b e allcomp)))

(with-eval-after-load 'graphviz-dot-mode
  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4)

  (define-hook! extra|setup-dot (graphviz-dot-mode-hook)
    (hs-minor-mode 1)
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions 'extra//dot-complete)))



(with-eval-after-load 'grep
  (setq grep-highlight-matches t
        grep-scroll-output t)
  (dolist (v core-ignored-directories)
    (add-to-list 'grep-find-ignored-directories v)))



;; Zeal at point
(global-set-key (kbd "C-h z") 'zeal-at-point)
(global-set-key (kbd "C-h Z") 'zeal-at-point-search)
(with-eval-after-load 'zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))



(defun extra/clipboard-copy (-beg -end)
  (interactive "r")
  (if (display-graphic-p)
      (kill-new (buffer-substring-no-properties -beg -end))
    (if env-has-xsel-p
        (if (= 0 (shell-command-on-region -beg -end "xsel -ib"))
            (message "Copy finished")
          (message "Error occured !!!"))
      (message "Executable `xsel' not found !!!"))))

(defun extra/clipboard-paste ()
  (interactive)
  (if (display-graphic-p)
      (yank 1)
    (if env-has-xsel-p
        (shell-command "xsel -ob" t)
      (message "Executable `xsel' not found !!!"))))



(with-eval-after-load 'skeletor
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir)
                      (skeletor-async-shell-command "mkdir build"))))
(global-set-key (kbd "C-x p n") 'skeletor-create-project)
(global-set-key (kbd "C-x p N") 'skeletor-create-project-at)



(defvar extra-translate-shell-repo "https://github.com/soimort/translate-shell")
(defvar extra-translate-shell-path
  (eval-when-compile (expand-var! "translate-shell")))
(defvar extra-translate-shell-args
  (eval-when-compile (split-string "-I -s zh -t en -e google" " ")))

(defun extra/translate-shell ()
  (interactive)
  (let ((exe (expand-file-name "build/trans" extra-translate-shell-path))
        (string (when (region-active-p)
                  (buffer-substring (region-beginning) (region-end)))))
    (cond
     ((not (file-exists-p exe))
      (message "Executable `%s' not found !" exe))
     ((not (file-executable-p exe))
      (message "`%s' can't be executed" exe))
     (t
      (let* ((buffer (get-buffer-create "*translate-shell*"))
             (proc (get-buffer-process buffer)))
        (if (equal (current-buffer) buffer)
            (quit-window)
          (when (not (and proc
                          (process-live-p proc)
                          (eq (buffer-local-value 'major-mode buffer) 'comint-mode)))
            (with-current-buffer buffer
              (let ((buffer-read-only nil))
                (erase-buffer))
              (comint-exec buffer "trans" exe nil extra-translate-shell-args)
              (comint-mode)
              (setq proc (get-buffer-process buffer))))
          (with-current-buffer (pop-to-buffer buffer)
            (comint-send-string proc (concat string "\n")))))))))

(defun extra/create-or-update-trans ()
  (interactive)
  (if (file-directory-p extra-translate-shell-path)
      (let ((default-directory extra-translate-shell-path))
        (compilation-start "git pull && git submodule update && make"))
    (compilation-start (format "git clone  %s %s && cd %s && make"
                               extra-translate-shell-repo
                               extra-translate-shell-path
                               extra-translate-shell-path))))

(define-key! :map core-ctrl-z-map
  ("i" . extra/translate-shell)
  ("I" . extra/create-or-update-trans))

(provide 'init-extra)

;;; -*- lexical-binding: t; -*-

(require-packages!
 octave
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
 restclient
 cython-mode
 flycheck-cython
 sly)



(add-auto-mode! 'crontab-mode "\\.?cron\\(tab\\)?\\'")

;; Star dictionary lookup
(autoload 'sdcv-current-word "sdcv" nil t)
(autoload 'sdcv-goto-sdcv "sdcv" nil t)

;; `calc' setup
(with-eval-after-load 'calc
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(with-eval-after-load 'octave
  (define-key! :map octave-mode-map
    ("C-c C-d" . octave-help)
    ("C-c C-c" . octave-send-buffer)
    ("C-c C-l" . octave-send-line)
    ("C-c C-e" . octave-send-defun)))

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

(define-hook! cython|setup (cython-mode-hook)
  (setq electric-indent-chars (delq ?: electric-indent-chars))

  (local-set-key (kbd "C-c C-b") nil)

  (when (buffer-enable-rich-feature-p)
    (flycheck-mode 1)))



(with-eval-after-load 'graphviz-dot-mode
  (require 'company-graphviz-dot)
  (remove-hook 'company-backends 'company-graphviz-dot-backend)

  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4)

  (define-hook! extra|setup-dot (graphviz-dot-mode-hook)
    (hs-minor-mode 1)
    (company//add-backend 'company-graphviz-dot-backend)))



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



(with-eval-after-load 'skeletor
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir)
                      (skeletor-async-shell-command "mkdir build"))))
(global-set-key (kbd "C-x p n") 'skeletor-create-project)
(global-set-key (kbd "C-x p N") 'skeletor-create-project-at)



(define-key! :map core-ctrl-z-map
  ("i" . extra/translate-shell)
  ("I" . extra/create-or-update-trans))

(provide 'init-extra)

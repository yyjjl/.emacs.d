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

(setq prolog-system 'swi) ;; prolog system
(setq erc-track-enable-keybindings nil) ;; erc
(setq pulse-delay 0.01) ;; pulse
(setq csv-separators '("," ";" "|" " ")) ;; csv
(setq inferior-lisp-program "sbcl")

;; `calc' setup
(config! calc
  :config
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(config! octave
  :bind
  (:map octave-mode-map
   ("C-c C-d" . octave-help)
   ("C-c C-c" . octave-send-buffer)
   ("C-c C-l" . octave-send-line)
   ("C-c C-e" . octave-send-defun)))

(config! cython-mode
  :hook
  (setup
   :define (cython-mode-hook)
   (setq electric-indent-chars (delq ?: electric-indent-chars))

   (local-set-key (kbd "C-c C-b") nil)

   (when (buffer-enable-rich-feature-p)
     (flycheck-mode 1))))

(config! graphviz-dot-mode
  :hook
  (setup
   :define (graphviz-dot-mode-hook)
   (hs-minor-mode 1)
   (company//add-backend 'company-graphviz-dot-backend))

  :config
  (require 'company-graphviz-dot)
  (remove-hook 'company-backends 'company-graphviz-dot-backend)

  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4))

(config! zeal-at-point
  :config
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))

(config! skeletor
  :config
  (setq skeletor-completing-read-function 'ivy-completing-read)

  (skeletor-define-template "cpp-cmake"
    :title "C++ Project (CMake)"
    :default-license "^gpl"
    :after-creation (lambda (dir) (skeletor-async-shell-command "mkdir build"))))

(define-key! :prefix "C-x w"
  ;; buffer-mode
  ("k" . buf-move-up)
  ("j" . buf-move-down)
  ("h" . buf-move-left)
  ("l" . buf-move-right))

(define-key! :prefix "C-x p"
  ("n" . skeletor-create-project)
  ("N" . skeletor-create-project-at))

(define-key! :prefix "C-h"
  ("z" . zeal-at-point)
  ("Z" . zeal-at-point-search))

(define-key! :map core-ctrl-z-map
  ("i" . extra/translate-shell)
  ("I" . extra/create-or-update-trans))

(provide 'init-extra)

(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
        'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))



(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (setq compilation-window-height 8)
  (add-to-list 'compilation-finish-functions
               '(lambda (buf str)
                  (if (string-match "exited abnormally" str)
                      ;;there were errors
                      (message "compilation errors, press C-x ` to visit")
                    ;;no errors, make the compilation window go away in 0.5 seconds
                    (when (string-match "*compilation*" (buffer-name buf))
                      ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
                      (bury-buffer "*compilation*")
                      (winner-undo)
                      (message "NO COMPILATION ERRORS!")
                      ))))

  ;;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  (ggtags-mode 1)
  (ggtags-auto-update-mode 1)
  ;; indent
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(defun my-c-mode-setup ()
  "C/C++ only setup"
  ;; (message "my-c-mode-setup called (buffer-file-name)=%s" (buffer-file-name))
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (local-set-key (kbd "C-c b") 'clang-format-buffer)
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "M-RET") 'srefactor-refactor-at-point)

  (semantic-mode 1)

  (flycheck-mode 1)

  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  (when buffer-file-name
    ;; c-eldoc (https://github.com/mooz/c-eldoc)
    (c-turn-on-eldoc-mode)

    ;; @see https://github.com/redguardtoo/cpputils-cmake
    ;; Make sure your project use cmake!
    ;; Or else, you need comment out below code:
    ;; {{

    ;; (if (executable-find "cmake")
    ;;     (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
    ;;                  (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
    ;;         (cppcm-reload-all)))
    ;; }}
    )
  )
;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (is-buffer-file-temp)
              (my-common-cc-mode-setup)
              (if (derived-mode-p 'c++-mode)
                  (setq-local flycheck-clang-language-standard "c++14")
                (setq flycheck-clang-language-standard nil))
              (unless (or (derived-mode-p 'java-mode)
                          (derived-mode-p 'groovy-mode))
                (my-c-mode-setup)))))

(with-eval-after-load 'bison-mode
  (setq bison-electric-open-brace-v nil))

(provide 'init-cc-mode)

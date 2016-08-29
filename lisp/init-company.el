(with-eval-after-load 'company
  ;; @see https://github.com/company-mode/company-mode/issues/348
     (unless (featurep 'company-statistics)
       (require 'company-statistics))
     (company-statistics-mode 1)

     (let ((bg (face-attribute 'default :background)))
       (custom-set-faces
        `(company-tooltip
          ((t (:inherit default :background ,(color-lighten-name bg 5)))))
        `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
        `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
        `(company-tooltip-selection
          ((t (:inherit font-lock-keyword-face
                        :background ,(color-lighten-name bg 20)))))
        `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

     ;; can't work with TRAMP
     (setq company-backends (delete 'company-ropemacs company-backends))
     (setq company-backends (delete 'company-semantic company-backends))
     ;; make company-files a work before capf
     (setq company-backends (delete 'company-files  company-backends))
     ;; (setq company-backends (delete 'company-capf company-backends))
     (add-to-list 'company-backends 'company-cmake)
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
     (add-to-list 'company-backends 'company-tern)
     (add-to-list 'company-backends 'company-web-html)
     (add-to-list 'company-backends 'company-shell)
     (add-to-list 'company-backends 'company-files)
;;     (add-to-list 'company-backends 'company-web-jade)
;;     (add-to-list 'company-backends 'company-web-slim)
     (add-to-list 'company-backends 'company-cabal)
     ;; I don't like the downcase word in company-dabbrev
     ;; for languages use camel case naming convention
     ;; company should be case sensitive
     (setq company-dabbrev-downcase nil)
     (setq company-dabbrev-ignore-case nil)
     (setq company-dabbrev-code-ignore-case t)
     (setq company-show-numbers t)
     (setq company-idle-delay 0.2)
     (setq company-clang-insert-arguments nil)
     (setq company-require-match nil)
     (setq company-etags-ignore-case t)
     (setq company-minimum-prefix-length 2)
     ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
     (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
       ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
       (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
           (setq ad-return-value nil)
         ad-do-it))

     ;; press SPACE will accept the highlighted candidate and insert a space
     ;; `M-x describe-variable company-auto-complete-chars` for details
     ;; That's BAD idea.
     (setq company-auto-complete nil)
     ;; NOT to load company-mode for certain major modes.
     ;; Ironic that I suggested this feature but I totally forgot it
     ;; until two years later.
     ;; https://github.com/company-mode/company-mode/issues/29
     (setq company-global-modes
           '(not
             eshell-mode comint-mode erc-mode gud-mode rcirc-mode
             minibuffer-inactive-mode)))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    (setq company-ispell-dictionary ispell-alternate-dictionary)))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
(add-hook 'org-mode-hook 'company-ispell-setup)
(add-hook 'text-mode-hook 'company-ispell-setup)
;; }}

(with-eval-after-load 'company-etags
  (add-to-list 'company-etags-modes 'web-mode))

(with-eval-after-load 'company-shell
  (setq company-shell-modes '(sh-mode)))

(provide 'init-company)

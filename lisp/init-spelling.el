(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")

(make-variable-buffer-local 'flyspell-check-doublon)

(with-eval-after-load 'ispell
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    ;; just reset dictionary to the safe one "en_US" for hunspell.
    ;; if we need use different dictionary, we specify it in command line arguments
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
   (t (setq ispell-program-name nil)
      (message "You need install either aspell or hunspell for ispell"))))

(with-eval-after-load 'flyspell
  ;; flyspell set up for web-mode
  (defun web-mode-flyspell-verify ()
    (let ((f (get-text-property (- (point) 1) 'face))
          thing
          rlt)
      (cond
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face)
                   ))
        (setq rlt t))
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq thing (thing-at-point 'symbol))
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$" thing))
          rlt))
       (t t))
      rlt))

  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

  ;; better performance
  (setq flyspell-issue-message-flag nil)

  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  ;; whatever spell checker I use, I always use English dictionary
  ;; I prefer use aspell because:
  ;; 1. aspell is older
  ;; 2. looks Kevin Atkinson still get some road map for aspell:
  ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
  (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
    "if RUN-TOGETHER is true, spell check the CamelCase words"
    (let (args)
      (when ispell-program-name
        (cond
         ((string-match "aspell$" ispell-program-name)
          ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
          (setq args (list "--sug-mode=ultra" "--lang=en_US"))
          (if RUN-TOGETHER
              (setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2")))))
         ((string-match "hunspell$" ispell-program-name)
          (setq args nil))))
      args))

  ;; ispell-cmd-args is useless, it's the list of *extra*
  ;; command line arguments we will append to the ispell
  ;; process when ispell-send-string()
  ;; ispell-extra-args is the command arguments which will
  ;; *always* be used when start ispell process
  (setq ispell-extra-args (flyspell-detect-ispell-args t))
  ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))


  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; use emacs original arguments
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      ;; restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; use emacs original arguments
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      ;; restore our own ispell arguments
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))
  (defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
      ad-do-it)))

;; Add auto spell-checking in comments for all programming language modes
;; if and only if there is enough memory
;; You can use prog-mode-hook instead.
(defun can-enable-flyspell-mode ()
  (and (not *no-memory*)
       ispell-program-name
       (executable-find ispell-program-name)))

(defun enable-flyspell-mode-conditionally ()
  (if (and (not *no-memory*)
           ispell-program-name
           (executable-find ispell-program-name))
      (flyspell-mode 1)))

(if (can-enable-flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(autoload 'flyspell-correct-ivy "flyspell-correct-ivy" nil t)
(setq flyspell-correct-interface 'flyspell-correct-ivy)

(bind-keys ("C-c s" . flyspell-correct-word-generic)
           ("C-c S" . flyspell-buffer)
           ("C-c 4" . ispell-word))

(provide 'init-spelling)

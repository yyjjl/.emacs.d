;;  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'dash))

(defvar ymacs-popup-occur-buffer-regexp
  (eval-when-compile
    (rx string-start "*"
        (or "Backtrace"
            "RefTeX Select"
            "LSP Lookup"
            "EGLOT proposed server changes"
            (and (or "xref" "TeX" "Shell Command Output" "lsp-java-") (* any)))
        "*" string-end)))

;; (defvar ymacs-popup-left-dedicated-buffer-regexp
;;   (eval-when-compile
;;     (rx string-start "*"
;;         (and "reftex" (*? not-newline))
;;         "*" string-end)))

(defvar ymacs-popup-below-autoclose-buffer-regexp
  (eval-when-compile
    (rx string-start
        (or "*Warnings*"
            "*Message*"
            "*Org Agenda*"
            "*Org Dashboard*"
            "*Org Select*"
            (and "*eldoc" (*? not-newline) "*")
            " *LanguageTool Errors* "
            " *CDLaTeX Help*")
        string-end)))

(defvar ymacs-popup-other-window-regexp
  (eval-when-compile
    (rx string-start
        "*" (or "Man" "WoMan") (*? not-newline) "*"
        string-end)))

(defvar ymacs-popup-help-buffer-regexp
  (eval-when-compile
    (rx string-start
        "*"
        (or "Compile-Log"
            "trace-output"
            (and "poporg: " (*? not-newline))
            "sdcv"
            "eldoc"
            "ymacs-lisp-message"
            (and (*? not-newline) (in "Hh") "elp" (*? not-newline))
            (and (*? not-newline) (in "Dd") "escribe" (*? not-newline))
            (and (*? not-newline) (in "Dd") "ocumentation" (*? not-newline)))
        "*"
        string-end)))

(defvar ymacs-popup-term-buffer-regexp
  (eval-when-compile
    (rx string-start
        "*"
        (or (and (*? not-newline) "shell")
            "prolog"
            "sage"
            (and (*? not-newline) (or "repl" "compilation") (*? not-newline)))
        (?  "<" (+ digit) ">")
        "*"
        (?  "<" (+ digit) ">")
        string-end)))

(defvar ymacs-popup--window-list nil)
(defvar ymacs-popup--term-buffer-list nil)

(defvar ymacs-popup-term-modes
  '(term-mode
    eshell-mode
    comint-mode
    vterm-mode
    haskell-interactive-mode))

(defvar ymacs-popup-help-modes
  '(help-mode
    compilation-mode
    messages-buffer-mode
    completion-list-mode
    TeX-output-mode
    TeX-error-overview-mode
    flymake-diagnostics-buffer-mode
    profiler-report-mode))

(defvar-local ymacs-popup--matched-rule nil)
(defvar-local ymacs-popup--nosplit-window nil)
(put 'ymacs-popup--matched-rule 'permanent-local t)
(put 'ymacs-popup--nosplit-window 'permanent-local t)

(defvar ymacs-popup-default-size 0.4)
(defvar ymacs-popup-default-side 'below)
(defvar ymacs-popup-max-slots 2)

(define-key!
  ("C-x 1" . ymacs-popup/delete-other-window)
  ("C-z" :map
   (define-key! :map (make-sparse-keymap)
     ("l" . ymacs-popup/last-popup-window)
     ("d" . sdcv-search-word)
     ("b" . pop-to-buffer)
     ("RET" . window-toggle-side-windows))))

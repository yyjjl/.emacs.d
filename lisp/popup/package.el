;;  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'dash))

(defvar ymacs-popup-default-regexp
  (rx string-start
      "*" (*? not-newline) (?  "<" (+ digit) ">") "*" (?  "<" (+ digit) ">")
      string-end))

(defvar ymacs-popup-other-window-regexp
  (rx string-start
      "*" (or "Man" "TeX" "Shell Command Output") (*? not-newline) "*"
      string-end))

(defvar ymacs-popup-help-buffer-regexp
  (rx string-start
      "*"
      (or "Compile-Log"
          (and "poporg: " (*? not-newline))
          "sdcv"
          "lispy-message"
          (and (*? not-newline) (in "Hh") "elp" (*? not-newline))
          (and (*? not-newline) (in "Dd") "escribe" (*? not-newline))
          (and (*? not-newline) (in "Dd") "ocumentation" (*? not-newline)))
      "*"
      string-end))

(defvar ymacs-popup-term-buffer-regexp
  (rx string-start
      "*"
      (or "shell"
          "prolog"
          "Sage"
          (and (*? not-newline) (in "Rr") "epl" (*? not-newline))
          ;; (and (*? not-newline) (in "Ee") "rror" (*? not-newline))
          )
      (?  "<" (+ digit) ">")
      "*"
      (?  "<" (+ digit) ">")
      string-end))

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
    flycheck-error-list-mode
    flymake-diagnostics-buffer-mode
    profiler-report-mode))

(defvar-local ymacs-popup--matched-rule nil)
(put 'ymacs-popup--matched-rule 'permanent-local t)

(defvar ymacs-popup-default-size 0.4)
(defvar ymacs-popup-default-side 'below)

(defvar ymacs-popup-rules
  `((:macth-fn ymacs-popup//help-buffer-p
     :side below
     :select t
     :autoclose t)
    (:macth-fn ymacs-popup//term-buffer-p
     :side below
     :select t
     :terminal t)
    (:name-regexp ,ymacs-popup-other-window-regexp
     :select t
     :autoclose t)
    (:mode occur-mode
     :select t)
    (:name "*Flycheck error messages*"
     :side below
     :select nil
     :size 0.3)
    (:name ("*Warnings*" "*Backtrace*")
     :side below
     :autoclose t)))


(define-key!
  ("C-z" :map
   (define-key! :map (make-sparse-keymap)
     ("l" . ymacs-popup/last-popup-window)
     ("d" . ymacs-popup/popup-sdcv)
     ("RET" . ymacs-popup/fix-popup-window))))



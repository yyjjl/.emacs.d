;;  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'dash)
  (require 'shackle))

(defvar ymacs-popups-default-regexp
  (rx string-start
      "*" (*? not-newline) (?  "<" (+ digit) ">") "*" (?  "<" (+ digit) ">")
      string-end))

(defvar ymacs-popups-other-window-regexp
  (rx string-start
      "*" (or "Man" "TeX" "Shell Command Output") (*? not-newline) "*"
      string-end))

(defvar ymacs-popups-help-buffer-regexp
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

(defvar ymacs-popups-comint-buffer-regexp
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

(defvar ymacs-popups--window-list nil)

(defvar-local ymacs-popups-current-window nil)

(put 'ymacs-popups-current-window 'permanent-local t)

(defvar ymacs-popups-comint-modes
  '(term-mode
    vterm-mode
    haskell-interactive-mode))

(defvar ymacs-popups-help-modes
  '(help-mode
    messages-buffer-mode
    completion-list-mode
    compilation-mode
    TeX-output-mode
    TeX-error-overview-mode
    flycheck-error-list-mode
    flymake-diagnostics-buffer-mode
    profiler-report-mode))

(defvar ymacs-ctrl-z-map
  (define-key! :map (make-sparse-keymap)
    ("l" . ymacs-popups/last-popup-window)
    ("d" . ymacs-popups/popup-sdcv)
    ("b" . ymacs-popups/display-buffer)
    ("RET" . ymacs-popups/fix-popup-window)))

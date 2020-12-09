;;; -*- lexical-binding: t; -*-

(executable! sage)

(require-packages!
 (sage-shell-mode :when ymacs-sage-path
                  :compile (sage-shell-mode sage-shell-view)))

(defconst ymacs-sage-overlay-help-template
  "--------------------
Text: %s
LaTeX: %s
--------------------
[R] Regenerate [T] Show text [w] Copy text
[W] Copy LaTeX [=] Zoom in   [-] Zoom out")

(defvar ymacs-sage-shell:source-buffer nil)


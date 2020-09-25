;;; -*- lexical-binding: t; -*-

(defvar ymacs-hideshow-overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defvar ymacs-hideshow-persistent-file "fold.el")

(defvar ymacs-hideshow-persistent-table (make-hash-table :test #'equal))

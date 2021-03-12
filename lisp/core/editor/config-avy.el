;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-avy-copy-key-alist
  '((?s . symbol)
    (?e . sexp)
    (?l . line)
    (?f . filename)
    (?d . defun)
    (?W . word)
    (?u . url)
    (?U . uuid)
    (?n . number)))

(after! avy
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m))

  (setq avy-background t)
  (setq avy-all-windows nil)
  (setq avy-all-windows-alt t)
  (setq avy-style 'at-full))

(after! ace-window
  (define-advice aw-update (:override () ignore-on)
    (let ((aw-ignore-on t)
          (aw-ignore-current))
      (avy-traverse
       (avy-tree (remove (minibuffer-window) (aw-window-list)) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize (apply #'string (reverse path)) 'face 'aw-mode-line-face))))))


  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6))

  (define-key!
    ("M-1" . ymacs-editor/aw-select-window)
    ("M-2" . ymacs-editor/aw-select-window)
    ("M-3" . ymacs-editor/aw-select-window)
    ("M-4" . ymacs-editor/aw-select-window)
    ("M-5" . ymacs-editor/aw-select-window)
    ("M-6" . ymacs-editor/aw-select-window))

  (setq aw-scope 'frame
        aw-display-mode-overlay nil
        aw-reverse-frame-list nil
        aw-dispatch-always nil))

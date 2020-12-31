;; -*- lexical-binding:t -*-

;; Begin of Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-feature! package)
(load-feature! ui)
(load-feature! popup)
(load-feature! completion-ivy)
(load-feature! hydra)
(load-feature! company)
(load-feature! checker)
(load-feature! tools)
(load-feature! debug)
(load-feature! term (type . 'vterm))
(load-feature! semantic)
(load-feature! hideshow)
(load-feature! edit)
(load-feature! dired)
(load-feature! ibuffer)
(load-feature! lisp)
;; End of Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-feature! lsp)

(load-feature! git)
(load-feature! spell)

(load-feature! latex)
(load-feature! org)
;; (load-feature! sage)
;; (load-feature! treemacs)

(load-feature! cpp)
(load-feature! cpp-cmake)
(load-feature! cpp-clangd)

(load-feature! python (lsp-server . 'pyright))

;; Programming modes
(load-feature! sh)
;; (load-feature! haskell)
;; (load-feature! rust)
;; (load-feature! java)
(load-feature! web)
;; (load-feature! go)
(load-feature! perl)
;; (load-feature! racket)

;; (load-feature! emms)
;; (load-feature! desktop)
;; (load-feature! completion-ivy-window)
;; (load-feature! lsp-ui)

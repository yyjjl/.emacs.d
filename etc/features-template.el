;; -*- lexical-binding:t -*-

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

(load-feature! python (lsp-server . 'pyls))

;; Programming modes
(load-feature! sh)
;; (load-feature! haskell)
;; (load-feature! rust)
(load-feature! web)
;; (load-feature! go)
;; (load-feature! java)
;; (load-feature! racket)
(load-feature! perl)

;; (load-feature! play)
;; (load-feature! emms)

;; (load-feature! desktop)
;; (load-feature! completion-ivy-window)
;; (load-feature! lsp-ui)

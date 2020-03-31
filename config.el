;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(load! "programming")
(load! "text")
(load! "bindings")

;; (let ((px (display-pixel-width))
;;   (py (display-pixel-height))
;;   (fx (frame-char-width))
;;   (fy (frame-char-height))
;;   tx ty)
;; (setq tx (- (/ px fx) 7))
;; (setq ty (- (/ py fy) 4))
;; (setq initial-frame-alist '((top . 2) (left . 2)))
;; (add-to-list 'default-frame-alist (cons 'width tx))
;; (add-to-list 'default-frame-alist (cons 'height ty)))


(add-to-list 'default-frame-alist '(fullscreen . maximized))

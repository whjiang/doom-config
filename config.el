;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
(load! "programming")
(load! "text")

 ;; Enable mouse support
(unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
    (setq mouse-wheel-up-event 'mouse-5)
    (setq mouse-wheel-down-event 'mouse-4))

(xterm-mouse-mode -1)
(setq mouse-drag-copy-region t)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(map! :leader
      (:prefix "c"                      ; code
	:desc "Switch .cpp/.h file"    "h" #'ff-find-other-file)
)


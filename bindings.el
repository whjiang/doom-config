;;; private/my/+bindings.el -*- lexical-binding: t; -*-


 ;; Enable mouse support
(unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
    (setq mouse-wheel-up-event 'mouse-5)
    (setq mouse-wheel-down-event 'mouse-4)
    (toggle-menu-bar-mode-from-frame))

(xterm-mouse-mode -1)
(setq mouse-drag-copy-region t)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;(when IS-MAC (setq mac-command-modifier 'meta
;;                   mac-option-modifier  'alt))


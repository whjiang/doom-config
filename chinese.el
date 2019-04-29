;;; pyim

;;(def-package! pyim
;;  :demand t
;;
;;  :config
;;  (use-package pyim-basedict
;;   :ensure nil
;;   :config (pyim-basedict-enable))
;;  (setq pyim-dcache-directory (expand-file-name "pyim/dcache" doom-local-dir)
;;        default-input-method "pyim"
;;        pyim-default-scheme 'quanpin)
;;
;;  (setq-default pyim-english-input-switch-functions
;;                '(pyim-probe-dynamic-english
;;                  pyim-probe-org-structure-template))
;;
;;  (setq-default pyim-punctuation-half-width-functions
;;                '(pyim-probe-punctuation-line-beginning
;;                  pyim-probe-punctuation-after-punctuation))
;;
;;  (setq pyim-page-tooltip 'popup
;;        pyim-page-length 5
;;        pyim-page-style 'vertical)
;;
;;  :bind
;;  (("M-j" . pyim-convert-code-at-point)
;;   ("C-;" . pyim-delete-word-from-personal-buffer))
;;  )

 (def-package! pyim
   :defer 1
   :config
   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
   (use-package pyim-basedict
     :ensure nil
     :config (pyim-basedict-enable))

   (setq default-input-method "pyim")

   ;; 我使用全拼
   (setq pyim-default-scheme 'quanpin)

   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
   ;; 我自己使用的中英文动态切换规则是：
   ;; 1. 光标只有在注释里面时，才可以输入中文。
   ;; 2. 光标前是汉字字符时，才能输入中文。
   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
   (setq-default pyim-english-input-switch-functions
                 '(pyim-probe-auto-english
                   pyim-probe-isearch-mode
                   pyim-probe-program-mode
                   pyim-probe-org-structure-template))

   (setq-default pyim-punctuation-half-width-functions
                 '(pyim-probe-punctuation-line-beginning
                   pyim-probe-punctuation-after-punctuation))

   ;; 开启拼音搜索功能
   (pyim-isearch-mode 1)

   ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
   ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
   ;; 手动安装 posframe 包。
   (setq pyim-page-tooltip 'popup)

   ;; 选词框显示5个候选词
   (setq pyim-page-length 5)

   :bind
   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
    ("C-;" . pyim-delete-word-from-personal-buffer)))

;; (after! pyim
   ;;(pyim-basedict-enable)
;;   (toggle-input-method)
;;   (run-with-timer 15 nil
;;                   (lambda () (cd "~"))))

;; (def-package! fcitx
;;   :defer t
;;   :config
;;   ;; Make sure the following comes before `(fcitx-aggressive-setup)'
;;   (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
;;   (fcitx-aggressive-setup)
;;   (fcitx-prefix-keys-add "C-x" "C-c" "C-h" "M-s" "M-o")
;;   (fcitx-prefix-keys-turn-on)
;;   )

;;(use-package pyim
;;  :ensure nil
;;  :demand t
;;  :config
;;  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;  (use-package pyim-basedict
;;    :ensure nil
;;    :config (pyim-basedict-enable))
;;
;;  (setq default-input-method "pyim")
;;
;;  ;; 我使用全拼
;;  (setq pyim-default-scheme 'quanpin)
;;
;;  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;  ;; 我自己使用的中英文动态切换规则是：
;;  ;; 1. 光标只有在注释里面时，才可以输入中文。
;;  ;; 2. 光标前是汉字字符时，才能输入中文。
;;  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;  (setq-default pyim-english-input-switch-functions
;;                '(pyim-probe-auto-english
;;                  pyim-probe-isearch-mode
;;                  pyim-probe-program-mode
;;                  pyim-probe-org-structure-template))
;;
;;  (setq-default pyim-punctuation-half-width-functions
;;                '(pyim-probe-punctuation-line-beginning
;;                  pyim-probe-punctuation-after-punctuation))
;;
;;  ;; 开启拼音搜索功能
;;  (pyim-isearch-mode 1)
;;
;;  ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
;;  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
;;  ;; 手动安装 posframe 包。
;;  (setq pyim-page-tooltip 'popup)
;;
;;  ;; 选词框显示5个候选词
;;  (setq pyim-page-length 5)
;;
;;  :bind
;;  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;   ("C-;" . pyim-delete-word-from-personal-buffer)))

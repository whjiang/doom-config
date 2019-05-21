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
;;(add-to-list 'load-path "~/.doom.d/liberime")
;;(require 'liberime)

 (def-package! pyim
   :defer 1
   :config
   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
   (use-package pyim-basedict
     :ensure nil
     :config (pyim-basedict-enable))

(use-package liberime
  :load-path "/Users/guobei/.doom.d/liberime"
  :init
  (setq liberime-shared-data-dir (expand-file-name "~/Library/Rime"))
  (setq liberime-user-data-dir (expand-file-name "rime/" doom-etc-dir))
;;  (load-file "/Users/jiya/workspace/liberime/liberime-config.el")
  :commands (liberime-start liberime-select-schema)
  :config
  (setq pyim-page-style 'one-line)
  (defun setup-liberime ()
    ;; incase hooks not running
    (interactive)
    (liberime-start liberime-shared-data-dir liberime-user-data-dir)
    (liberime-select-schema "cqkm")
    (setq pyim-default-scheme 'rime)
    )

  ;; work with pyim
  (add-hook 'pyim-load-hook 'setup-liberime) ;; or set with use-package
  )

(use-package liberime
  :load-path "/Users/guobei/.doom.d/liberime"
  :config
  ;; 注意事项:
  ;; 1. 文件路径需要用 `expand-file-name' 函数处理。
  ;; 2. `librime-start' 的第一个参数说明 "rime 共享数据文件夹"
  ;;     的位置，不同的平台其位置也各不相同，可以参考：
  ;;     https://github.com/rime/home/wiki/RimeWithSchemata
  ;;(liberime-start (expand-file-name "/Library/Input Methods/Squirrel.app/Contents/SharedSupport/")
  (liberime-start (expand-file-name "~/Library/Rime")
		  (expand-file-name "rime/" doom-etc-dir))
                  (liberime-select-schema "cqkm")
                 (setq pyim-default-scheme 'rime))

;; work with pyim
(add-hook 'pyim-load-hook 'setup-liberime) ;; or set with use-package

   ;;(setq pyim-dcache-directory (expand-file-name "pyim/dcache" doom-local-dir))
   (setq default-input-method "pyim")

;;   (add-to-list  'pyim-schemes  '(cqkm
;;     :document "超强快码。"
;;     :class xingma
;;     :first-chars "abcdefghijklmnopqrstuvwxyz"
;;     :rest-chars "abcdefghijklmnopqrstuvwxyz"
;;     :code-prefix "+" ;五笔词库中所有的 code 都以 "+" 开头，防止和拼音词库冲突。
;;     :code-split-length 5 ;默认将用户输入切成 4 个字符长的 code 列表（不计算 code-prefix）
;;     :code-maximum-length 6 ;词库中，code 的最大长度（不计算 code-prefix）
;;     :prefer-trigger-chars nil))
;;   ;; 我使用全拼
;;   ;;(setq pyim-default-scheme 'cqkm)
;;   (setq pyim-auto-select t)
;;   (setq pyim-assistant-scheme-enable t)

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

   (setf pyim-fuzzy-pinyin-alist '(("en" "eng") ("in" "ing") ("z" "zh") ("c" "ch") ("s" "sh") ("r" "l")))

   ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
   ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
   ;; 手动安装 posframe 包。
   (setq pyim-page-tooltip 'posframe)

   ;; 选词框显示5个候选词
   (setq pyim-page-length 5)

   :bind
   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
    ("C-;" . pyim-delete-word-from-personal-buffer)))
;;
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

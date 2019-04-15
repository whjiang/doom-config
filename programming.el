;;; ~/.doom.d/programming.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))


(def-package! company-lsp
  :after company
  :init
  ;; Language servers have better idea filtering and sorting,
  ;; don't filter results on the client side.
  (setq company-transformers nil
        company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! cc-mode
  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setq c-default-style "my-cc")

  (setq-default c-basic-offset 2)

  (add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! python
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        importmagic-python-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8")

  ;; Resolve pylint cannot find relative PYTHONPATH issue
  (add-hook! python-mode (setenv "PYTHONPATH" (doom-project-root)))

  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")
  )


(def-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun spacemacs//python-sort-imports ()
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'spacemacs//python-sort-imports nil t)))


(def-package! importmagic
  :defer t
  :hook (python-mode . importmagic-mode)
  :commands (importmagic-fix-imports importmagic-fix-symbol-at-point))


(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (advice-add func :after #'reset-flycheck)))


(after! conda
  (setq conda-env-home-directory (expand-file-name "~/.conda"))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (advice-add func :after #'reset-flycheck))

  (setq conda-message-on-environment-switch nil)
  (conda-env-autoactivate-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! indium
  :defer t)


(def-package! import-js
  :defer t
  :init
  (add-hook! (js2-mode rjsx-mode)
    (add-hook 'after-save-hook #'import-js-fix nil t)))
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)


(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! go-mode
  (add-hook! go-mode (setq indent-tabs-mode nil))
  (add-hook! go-mode #'lsp))


;;add protobuf file detection
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.pb\\'" . protobuf-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cspell-base-program (executable-find "cspell"))
(defvar cspell-config-file-path (concat "'" (expand-file-name  "~/Dotfiles/cspell.json") "'"))
(defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
(defun cspell-check-buffer ()
  (interactive)
  (if cspell-base-program
      (let* ((file-name (concat "'" (buffer-file-name) "'"))
             (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")
    ))

(defun cspell-check-directory ()
  (interactive)
  (if cspell-base-program
      (let* ((files "'**/*.{js,jsx,ts,tsx,c,cc,cpp,h,hh,hpp,go,json}'")
             (command (string-join `(,cspell-base-program ,cspell-args ,files) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")))


;; (def-package! wucuo
;;   :defer t
;;   :init
;;   (add-hook! (js2-mode rjsx-mode go-mode c-mode c++-mode) #'wucuo-start))


(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  javascript-jshint handlebars
                  json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  python-pylint
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; ======================== JS & TS ========================
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . jsx-tide) 'append)
    (flycheck-add-next-checker 'typescript-tslint '(t .  typescript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append))

  ;; ======================== CC ========================
  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-add-next-checker 'c/c++-gcc '(t . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc-7"
        flycheck-gcc-include-path '("/usr/local/inclue"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++11"
          flycheck-clang-language-standard "c++11"))
  )

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! lsp-ui
  :defer t
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-ignore-duplicate t))

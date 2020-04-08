;;; ~/.doom.d/programming.el -*- lexical-binding: t; -*-


(after! clipetty
	(when (not (display-graphic-p))
	  (global-clipetty-mode)
	  )
	)

(after! tramp
	'(setenv "SHELL" "/bin/bash")
	;;(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
	(setq tramp-debug-buffer t)
(setq tramp-verbose 10)
	(add-to-list 'tramp-connection-properties
	     (list ".*" "locale" "LC_ALL=C")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))


(use-package! company-lsp
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

(after! (lsp-mode ccls)
(setq lsp-print-io t)

;;(ccls-executable (executable-find "ccls")) ; Add ccls to path if you haven't done so
;;(ccls-sem-highlight-method 'font-lock)
;;(ccls-enable-skipped-ranges nil)
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tramp-connection (lambda () (list* "/home/guobei.jwh/app/bin/ccls" ccls-args)))
  :major-modes '(c-mode c++-mode cuda-mode objc-mode)
  :server-id 'ccls-remote
  :multi-root nil
  :remote? t
  :notification-handlers
  (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
          ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
  :initialization-options (lambda () ccls-initialization-options)
  :library-folders-fn nil))
)


(custom-set-variables
 '(tramp-default-method "ssh")
 '(tramp-default-user "guobei.jwh")
 '(tramp-default-host "11.163.188.81#35829"))


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


(use-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun spacemacs//python-sort-imports ()
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'spacemacs//python-sort-imports nil t)))


(use-package! importmagic
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

(use-package! indium
  :defer t)


(use-package! import-js
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


;; (use-package! wucuo
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
  ;;(require 'flycheck-google-cpplint)
  ;;(setq flycheck-c/c++-googlelint-executable "cpplint")
  ;;(flycheck-add-next-checker 'c/c++-gcc '(t . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc-7"
        flycheck-gcc-include-path '("/usr/local/inclue"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++14"
          flycheck-clang-language-standard "c++14"))
  )

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! lsp-ui
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

;;find the top directory has CMakeLists.txt and create a build directory under it
(defun find-outermost-cmake-builddir ()
  "Finds the build directory of a CMake project"
  (let ((cmakefile nil) (dirname default-directory))
    ; Step 1: Find the CMakeFile in the parent directorie
    (setq depth 10)
    (while (and (not (string= dirname "/")) (> depth 0))
      (setq depth (1- depth))
      (setq detect (concat (file-name-as-directory dirname) "CMakeLists.txt"))
      (if (file-exists-p detect)
           (setq cmakefile detect))
      (setq dirname (expand-file-name (concat (file-name-as-directory dirname) ".."))))
    ; Step 2: Check if the build directory is there, and if so,
    ; whether there's a Makefile in it (i.e. cmake has been run)
    (if cmakefile
      (progn
        (message "Found CMakeLists.txt at %s" cmakefile)
        (let ((builddir (concat (file-name-directory cmakefile) "build")))
          (if (not (file-exists-p builddir))
              (mkdir builddir))
          (set (make-local-variable 'compile-command) (concat "cd " (shell-quote-argument builddir) " && cmake .. && make -j 4")))))))

(defun find-cmake-builddir ()
  "Finds the build directory of a CMake project"
  (let ((cmakefile nil) (dirname default-directory))
	; Step 1: Find the CMakeFile in the parent directories
	; Abort if found or if root dir is reached.
    (while (and (not (string= dirname "/")) (not cmakefile))
      (setq cmakefile (concat (file-name-as-directory dirname) "CMakeLists.txt"))
      (if (not (file-exists-p cmakefile))
	  (progn
	    (setq cmakefile nil)
	    (setq dirname (expand-file-name (concat (file-name-as-directory dirname) ".."))))))
	; Step 2: Check if the build directory is there, and if so,
	; whether there's a Makefile in it (i.e. cmake has been run)
    (if cmakefile
      (progn
        (message "Found CMakeLists.txt at %s" cmakefile)
        (let ((builddir (concat (file-name-directory cmakefile) "build")))
          (if (not (file-exists-p builddir))
              (mkdir builddir))
          (set (make-local-variable 'compile-command) (concat "cd " (shell-quote-argument builddir) " && cmake .. && make -j 4")))))))

(defun set-make-cmd ()
  "Set the build command to build.sh"
  (setq (project-root (projectile-project-root)))
  (if project-root
          (set (make-local-variable 'compile-command) (concat (shell-quote-argument project-root)  "/build.sh"))
  ))

;;(add-hook 'c-mode-common-hook 'set-make-cmd)
;;(require 'compile)
 ;; (add-hook 'c-mode-hook
 ;;           (lambda ()
 ;;             (set (make-local-variable 'compile-command)
 ;;                  "cd ${PWD%/src/*} && mkdir -p build && cd build && cmake .. && make -j 4"
 ;;                )))
 ;; (add-hook 'c++-mode-hook
 ;;           (lambda ()
 ;;             (set (make-local-variable 'compile-command)
 ;;                  "cd ${PWD%/src/*} && mkdir -p build && cd build && cmake .. && make -j 4"
 ;;                )))

;;c++-mode-hook

;;For all programming modes
;;treat underscore(_) as part of a word to be consistent with VIM
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;;avoid next-error in compilation to split window
(setq split-width-threshold nil)

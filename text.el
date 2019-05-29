;;; ~/.doom.d/text.el -*- lexical-binding: t; -*-

;;;  -*- lexical-binding: t; -*-

(setq evil-want-fine-undo 'yes)

(add-hook! 'text-mode-hook (setq-local truncate-lines nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq org-directory (expand-file-name "~/Dropbox/Org-Notes")
;;      org-agenda-files (list org-directory)
;;      org-ellipsis " ▼ "

      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
;;      org-bullets-bullet-list '("#"))

(after! org                          
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline "~/org/todo.org" "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline "~/org/personal.org" "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("p" "Project notes" entry
           (file+headline "~/org/project.org" "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
	))

   (setq org-log-into-drawer "LOGBOOK")


  ;; Schedule/deadline popup with default time
  (defvar org-default-time "10:30"
    "The default time for deadlines.")

  (defun advise-org-default-time (func arg &optional time)
    (let ((old-time (symbol-function #'org-read-date)))
      (cl-letf (((symbol-function #'org-read-date)
                 #'(lambda (&optional a b c d default-time f g)
                     (let ((default-time (or default-time
                                             org-default-time)))
                       (apply old-time a b c d f default-time g)
                       ))))
        (apply func arg time))))

  (advice-add #'org-deadline :around #'advise-org-default-time)
  (advice-add #'org-schedule :around #'advise-org-default-time)

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-refile-targets
        '(
          ("project.org" :maxlevel . 1)
          ("reading.org" :maxlevel . 1)
          ("personal.org" :maxlevel . 1)))
  (when IS-MAC
    (find-file "~/org/project.org")
    (find-file "~/org/personal.org")
    (find-file "~/org/reading.org")
    (switch-to-buffer "*doom*")
    )

  (setq org-agenda-text-search-extra-files (file-expand-wildcards "~/org/*.org_archive"))
  ;; (setq org-agenda-custom-commands
  ;; '(("n" . "Search in notes")
  ;;   ("nt" "Note tags search" tags ""
  ;;    ((org-agenda-text-search-extra-files (file-expand-wildcards "*.org_archive"))))
  ;;   ("ns" "Note full text search" search ""
  ;;    ((org-agenda-text-search-extra-files (file-expand-wildcards "*.org_archive"))))))
  ;;(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;;(setq org-refile-use-outline-path 'file)
  ;;(setq org-outline-path-complete-in-steps nil)
)


(def-package! org-wild-notifier
  :defer t
  :init
  (add-hook 'doom-post-init-hook #'org-wild-notifier-mode t)
  :config
  (setq org-wild-notifier-alert-time 15
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! markdown-mode
  ;; memo: install grip > ‘pip3 install grip‘
  (defvar +my/markdown-process nil)
  (defun +my/markdown-preview (&rest _)
    "Preview markdown file by using grip."
    (when (process-live-p +my/markdown-process)
      (kill-process +my/markdown-process))
    (setq +my/markdown-process
          (start-process-shell-command "grip markdown-preview"
                                       markdown-output-buffer-name
                                       (format "grip --browser '%s'" (buffer-file-name)))))

  ;; OVERRIDE
  (when (executable-find "grip")
    (advice-add #'markdown-preview :override #'+my/markdown-preview))
  )

(def-package! edit-indirect :defer t)
(def-package! protobuf-mode :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! youdao-dictionary
  :defer t
  :config
  ;; Enable Cache
  (setq url-automatic-caching t
        ;; Set file path for saving search history
        youdao-dictionary-search-history-file
        (concat doom-cache-dir ".youdao")
        ;; Enable Chinese word segmentation support
        youdao-dictionary-use-chinese-word-segmentation t))

(def-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t)
  )

(def-package! link-hint :defer t)

(def-package! symbol-overlay :defer t)

;;auto save org files
(add-hook 'focus-out-hook
        (lambda () (org-save-all-org-buffers)))

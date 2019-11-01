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

   (add-to-list 'org-emphasis-alist
                '("*" (:foreground "red")
                  ))
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

  (setq org-image-actual-width nil)

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

  (defun forward-and-preview ()
    (interactive)
    "Go to same level next heading and show preview in dedicated buffer"
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )
  (defun back-and-preview ()
    (interactive)
    "Go to same level previous heading and show preview in dedicated buffer"
    (hide-subtree)
    (org-speed-move-safe (quote outline-previous-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )
  (defun up-back-and-preview ()
    (interactive)
    "Go to previous level heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-up-heading))
    (org-tree-to-indirect-buffer)
    (hide-subtree)
    )
  (defun up-forward-and-preview ()
    (interactive)
    "Go to previous level next heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-up-heading))
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (org-tree-to-indirect-buffer)
    )
  (defun inside-and-preview ()
    (interactive)
    "Go to next level heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )
  (defhydra org-nav-hydra (:hint nil)
    "
         _k_
      _h_     _l_
         _j_
    "
    ("h" up-back-and-preview)
    ("j" forward-and-preview)
    ("k" back-and-preview)
    ("l" inside-and-preview)
    ("J" up-forward-and-preview "up forward")
    ("K" up-back-and-preview "up backward")
    ("q" winner-undo "quit" :exit t)
    )
  (defun org-nav ()
    (interactive)
    "Fold everything but the current heading and enter org-nav-hydra"
    (org-overview)
    (org-reveal)
    (org-show-subtree)
    (org-tree-to-indirect-buffer)
    (org-nav-hydra/body)
    )
  (map! :leader (:prefix ("m" . "localleader") "n" #'org-nav))

  ;;auto save org files
  (add-hook 'focus-out-hook
   (lambda () (org-save-all-org-buffers)))
)


(use-package! org-wild-notifier
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

(use-package! edit-indirect :defer t)
(use-package! protobuf-mode :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! youdao-dictionary
  :defer t
  :config
  ;; Enable Cache
  (setq url-automatic-caching t
        ;; Set file path for saving search history
        youdao-dictionary-search-history-file
        (concat doom-cache-dir ".youdao")
        ;; Enable Chinese word segmentation support
        youdao-dictionary-use-chinese-word-segmentation t))

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t)
  )

(use-package! link-hint :defer t)

(use-package! symbol-overlay :defer t)


(defun my/open-tree-view ()
  "Open a clone of the current buffer to the left, resize it to 30 columns, and bind <mouse-1> to jump to the same position in the base buffer."
  (interactive)
  (let ((new-buffer-name (concat "<tree>" (buffer-name))))
    ;; Create tree buffer
    (split-window-right 30)
    (if (get-buffer new-buffer-name)
        (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
      ;; Make new tree buffer
      (progn  (clone-indirect-buffer new-buffer-name nil t)
              (switch-to-buffer new-buffer-name)
              (read-only-mode)
              (hide-body)
              (toggle-truncate-lines)

              ;; Do this twice in case the point is in a hidden line
              (dotimes (_ 2 (forward-line 0)))

              ;; Map keys
              (use-local-map (copy-keymap outline-mode-map))
              (local-set-key (kbd "q") 'delete-window)
              (mapc (lambda (key) (local-set-key (kbd key) 'my/jump-to-point-and-show))
                    '("<mouse-1>" "RET"))))))

(defun my/jump-to-point-and-show ()
  "Switch to a cloned buffer's base buffer and move point to the cursor position in the clone."
  (interactive)
  (let ((buf (buffer-base-buffer)))
    (unless buf
      (error "You need to be in a cloned buffer!"))
    (let ((pos (point))
          (win (car (get-buffer-window-list buf))))
      (if win
          (select-window win)
        (other-window 1)
        (switch-to-buffer buf))
      (when (invisible-p (point))
        (show-branches))
      (widen)
      (goto-char pos)
      (org-narrow-to-element)
      (org-show-children)
      )))

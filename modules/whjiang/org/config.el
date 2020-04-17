;;; ~/.doom.d/modules/whjiang/org/config.el -*- lexical-binding: t; -*-

;; (add-hook 'org-mode-hook #'evil-normalize-keymaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org

  (global-set-key (kbd "<f12>") 'org-agenda)

  (defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

  (defun my/org-agenda-done (&optional arg)
    "Mark current TODO as done.
     This changes the line at point, all other lines in the agenda referring to
     the same tree node, and the headline of the tree node in the Org-mode file."
    (interactive "P")
    (org-agenda-todo "DONE")
    (org-clock-out-if-current)
    )

  (defun my/org-agenda-mode-fn ()
    ;;(evil-set-initial-state 'org-agenda-mode 'emacs)
    (define-key org-agenda-mode-map "x" 'my/org-agenda-done)
    (hl-line-mode 1)
    )
;;  (set-face-attribute 'hl-line nil :foreground nil :background "RoyalBlue4")
  (add-hook 'org-agenda-mode-hook #'my/org-agenda-mode-fn)

  ;;automatic set the captured item's tags according to the input
  ;; (defun my/org-auto-tag ()
  ;; (interactive)
  ;; (let ((alltags (append org-tag-persistent-alist org-tag-alist))
  ;;       (headline-words (split-string (org-get-heading t t)))
  ;;       )
  ;;   (mapcar (lambda (word) (if (assoc word alltags)
  ;;                            (org-toggle-tag word 'on)))
  ;;           headline-words))
  ;;   )
  ;; (add-hook 'org-capture-before-finalize-hook #'my/org-auto-tag())
 
;; Code based mostly on file+headline part of org-capture-set-target-location
;; Look for a headline that matches whatever *my-journal-headline-format* is
;; If it's not there insert it; otherwise position the cursor at the end of the
;; subtree.
;; (defconst *my-journal-headline-format* "WorkLog %Y-%m-%d %A")
;; (defun jww/find-journal-tree ()
;;   "Find or create my default journal tree"
;;   (setq hd (format-time-string *my-journal-headline-format*))
;;   (goto-char (point-min))
;;   (unless (derived-mode-p 'org-mode)
;;     (error
;;      "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
;;      (current-buffer)))
;;   (if (re-search-forward
;;        (format org-complex-heading-regexp-format (regexp-quote hd))
;;        nil t)
;;       (goto-char (point-at-bol))
;;     (goto-char (point-min))
;;     (or (bolp) (insert "\n"))
;;     (insert "* " hd "\n")
;;     (beginning-of-line 0))
;;   (org-end-of-subtree))

  (setq org-capture-templates
      (quote (("t" "Project todo" entry (file+headline "project.org" "Inbox")
               "* TODO %^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
              ("n" "Project note" entry (file+headline "project.org" "Inbox")
               "* NOTE %^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
              ("f" "Personal todo" entry (file+headline "personal.org" "Inbox")
               "* TODO %^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
;;              ("j" "Work log" entry (file+headline "project.org" "WorkLog")
;;               "* NOTE -%Y-%m-%d%^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
              ;; note the use of "plain" instead of "entry"; using "entry" made this a top-level
              ;; headline regardless of how many * I put in the template string (wtf?).
              ("j" "Journal(done)" entry (file+olp+datetree "diary.org" "Work Log")
               "** DONE %^{Description} \t%U \t:me:\nSCHEDULED: %T\n%?\n" :kill-buffer t)
              ;; "** DONE %<%H:%M> - %?\n" :kill-buffer t)
              ("d" "Journal(doing)" entry (file+olp+datetree "diary.org" "Work Log")
               "** DOING %^{Description} \t%U \t:me:\nSCHEDULED: %T\n%?\n" :clock-in t :clock-keep t)
               ;;"** DOING %<%Y-%m-%d %H:%M> - %^{Description} \t:me:\n%?\nSCHEDULED: %T\n" :clock-in t :clock-keep t)
              ("i" "Interview" entry (file+headline "project.org" "Hiring")
               "* NOTE %^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
              ("m" "Meeting Notes" entry (file+headline "project.org" "Meeting")
               "* %<%Y-%m-%d %H:%M> - 会议 %^{Description}\n%?\n\n:LOGBOOK:\n:Added: %U\n:END:\n\n" :prepend t :kill-buffer t)
;;              ("j" "Journal entry" entry (function org-journal-find-location)
;;                               "* %(format-time-string org-journal-time-format) %^{Title}\n%i%?" :kill-buffer t)
              )))
  ;; (setq org-capture-templates
  ;;       '(("t" "Peroject Todo" entry
  ;;          (file+headline "~/org/todo.org" "Inbox")
  ;;          "* TODO %?\n%i" :prepend t :kill-buffer t)
  ;;         ("n" "Personal notes" entry
  ;;          (file+headline "~/org/personal.org" "Inbox")
  ;;          "* %u %?\n%i" :prepend t :kill-buffer t)
  ;;         ("p" "Project notes" entry
  ;;          (file+headline "~/org/project.org" "Inbox")
  ;;          "* %u %?\n%i" :prepend t :kill-buffer t)
	;; ))
   
   (setq org-directory (expand-file-name "~/org")
      org-agenda-files (list org-directory)
      )

   (setq org-log-done 'time)
   (setq org-log-into-drawer "LOGBOOK")

   (setq org-journal-dir "~/org/journal"
	 org-journal-file-format "diary-%Y-%m-%d"
	 org-journal-date-prefix "#+TITLE: "
	 org-journal-date-format "%Y-%m-%d %A"
	 org-journal-time-prefix "* "
	 org-journal-time-format "%H:%M"
	 org-journal-enable-agenda-integration t
	 )
   ;;(add-to-list 'org-agenda-files org-journal-dir)
   
   (add-to-list 'org-emphasis-alist
                '("*" (:foreground "red")
                  ))
  ;; Schedule/deadline popup with default time
  (defvar org-default-time "10:30"
    "The default time for deadlines.")

  ;; (defun advise-org-default-time (func arg &optional time)
  ;;   (let ((old-time (symbol-function #'org-read-date)))
  ;;     (cl-letf (((symbol-function #'org-read-date)
  ;;                #'(lambda (&optional a b c d default-time f g)
  ;;                    (let ((default-time (or default-time
  ;;                                            org-default-time)))
  ;;                      (apply old-time a b c d f default-time g)
  ;;                      ))))
  ;;       (apply func arg time))))

  ;; (advice-add #'org-deadline :around #'advise-org-default-time)
  ;; (advice-add #'org-schedule :around #'advise-org-default-time)

  (setq org-image-actual-width (/ (display-pixel-width) 3))

  ;; Popup rules
  ;; Make org-agenda pop up to right of screen, 45% of width
  (set-popup-rule! "^\\*Org Agenda" :side 'right :size 0.45 :select t :ttl nil)
  ;; Same for org-ql
  (set-popup-rule! "^\\*Org QL" :side 'right :size 0.40 :select t :ttl nil)

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "ASSIGNED(a)" "DOING(i)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-refile-targets
        '(
          ("diary.org" :maxlevel . 1)
          ("project.org" :maxlevel . 1)
          ("personal.org" :maxlevel . 1)))
  ;; (when IS-MAC
  ;;   (find-file-read-only "~/org/project.org")
  ;;   (find-file-read-only "~/org/personal.org")
  ;;   (switch-to-buffer "*doom*")
  ;;   )

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

  ;; (defun forward-and-preview ()
  ;;   (interactive)
  ;;   "Go to same level next heading and show preview in dedicated buffer"
  ;;   (hide-subtree)
  ;;   (org-speed-move-safe (quote outline-next-visible-heading))
  ;;   (show-children)
  ;;   (org-tree-to-indirect-buffer)
  ;;   )
  ;; (defun back-and-preview ()
  ;;   (interactive)
  ;;   "Go to same level previous heading and show preview in dedicated buffer"
  ;;   (hide-subtree)
  ;;   (org-speed-move-safe (quote outline-previous-visible-heading))
  ;;   (show-children)
  ;;   (org-tree-to-indirect-buffer)
  ;;   )
  ;; (defun up-back-and-preview ()
  ;;   (interactive)
  ;;   "Go to previous level heading and show preview in dedicated buffer"
  ;;   (org-speed-move-safe (quote outline-up-heading))
  ;;   (org-tree-to-indirect-buffer)
  ;;   (hide-subtree)
  ;;   )
  ;; (defun up-forward-and-preview ()
  ;;   (interactive)
  ;;   "Go to previous level next heading and show preview in dedicated buffer"
  ;;   (org-speed-move-safe (quote outline-up-heading))
  ;;   (hide-subtree)
  ;;   (org-speed-move-safe (quote outline-next-visible-heading))
  ;;   (org-tree-to-indirect-buffer)
  ;;   )
  ;; (defun inside-and-preview ()
  ;;   (interactive)
  ;;   "Go to next level heading and show preview in dedicated buffer"
  ;;   (org-speed-move-safe (quote outline-next-visible-heading))
  ;;   (show-children)
  ;;   (org-tree-to-indirect-buffer)
  ;;   )
  ;; (defhydra org-nav-hydra (:hint nil)
  ;;   "
  ;;        _k_
  ;;     _h_     _l_
  ;;        _j_
  ;;   "
  ;;   ("h" up-back-and-preview)
  ;;   ("j" forward-and-preview)
  ;;   ("k" back-and-preview)
  ;;   ("l" inside-and-preview)
  ;;   ("J" up-forward-and-preview "up forward")
  ;;   ("K" up-back-and-preview "up backward")
  ;;   ("q" winner-undo "quit" :exit t)
  ;;   )
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
;;  (add-hook 'focus-out-hook
;;   (lambda () (org-save-all-org-buffers)))
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
)


(use-package! org-wild-notifier
  :defer t
  :init
  (add-hook 'doom-post-init-hook #'org-wild-notifier-mode t)
  :config
  (setq org-wild-notifier-alert-time 15
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)))


;; (def-package! org-super-agenda
;;  :after org-agenda
;;  :init
;;  (setq org-agenda-skip-scheduled-if-done t
;;      org-agenda-skip-deadline-if-done t
;;      org-agenda-include-deadlines t
;;      org-agenda-block-separator nil
;;      org-agenda-compact-blocks t
;;      org-agenda-start-day nil ;; i.e. today
;;      org-agenda-span 1
;;      org-agenda-start-on-weekday nil)
;;  (setq org-agenda-custom-commands
;;        '(("c" "Super view"
;;           ((agenda "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '((:name "Today"
;;                                  :time-grid t
;;                                  :date today
;;                                  :order 1)))))
;;            (alltodo "" ((org-agenda-overriding-header "")
;;                         (org-super-agenda-groups
;;                          '((:log t)
;;                            (:name "To refile"
;;                                   :file-path "refile\\.org")
;;                            (:name "Next to do"
;;                                   :todo "NEXT"
;;                                   :order 1)
;;                            (:name "Important"
;;                                   :priority "A"
;;                                   :order 6)
;;                            (:name "Today's tasks"
;;                                   :file-path "journal/")
;;                            (:name "Due Today"
;;                                   :deadline today
;;                                   :order 2)
;;                            (:name "Scheduled Soon"
;;                                   :scheduled future
;;                                   :order 8)
;;                            (:name "Overdue"
;;                                   :deadline past
;;                                   :order 7)
;;                            (:name "Meetings"
;;                                   :and (:todo "MEET" :scheduled future)
;;                                   :order 10)
;;                            (:discard (:not (:todo "TODO")))))))))))
;;  :config
;;  (org-super-agenda-mode))

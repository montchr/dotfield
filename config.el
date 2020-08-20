;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Montgomery"
      user-mail-address "chris@montchr.io")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15))

;; Change theme based on macOS light/dark mode.
(add-hook 'ns-system-appearance-change-functions
          #'(lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light (load-theme 'doom-one-light t))
                ('dark (load-theme 'doom-monokai-pro t)))))

(setq org-directory "~/org")

;; org-capture settings
(setq org-capture-todo-file "inbox.org")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Simple settings.
;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq undo-limit 80000000
      evil-want-fine-undo nil
      truncate-string-ellipsis "…")

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq ns-right-alternate-modifier 'none)

;; Autosave
(setq auto-save-default t
      auto-save-no-message t)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; Add prompt to select buffer upon opening new window
;; https://tecosaur.github.io/emacs-config/config.html#windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Show previews in ivy.
(setq +ivy-buffer-preview t)

;; List magit branches by date.
(setq magit-list-refs-sortby "-creatordate")

(setq cdom/org-agenda-directory "~/org/gtd/")

;; Attempts to prevent vterm from loading emacs from within itself,
;; but DOESN'T WORK!
(use-package! with-editor
  :general
  ([remap async-shell-command] 'with-editor-async-shell-command)
  ([remap shell-command] 'with-editor-shell-command)
  :hook
  (shell-mode . with-editor-export-editor)
  (term-exec  . with-editor-export-editor)
  (eshell-mode . with-editor-export-editor)
  (vterm-mode . with-editor-export-editor))

(after! org
  (defun cdom/org-archive-done-tasks ()
    "Archive all completed tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp)
  (setq org-agenda-files (find-lisp-find-files cdom/org-agenda-directory "\.org$")
        org-log-refile 'time))

(use-package! doct
  :commands (doct))

(after! doct
  (defun +doct-org-roam (groups)
    (let (converted)
      (dolist (group groups)
        (let* ((props (nthcdr 5 group))
               (roam-properties (plist-get (plist-get props :doct) :org-roam)))
          (push `(,@group ,@roam-properties) converted)))
      (setq doct-templates (nreverse converted))))
  (setq doct-after-conversion-functions '(+doct-org-roam)))

;; (setq org-capture-templates
;;       (doct `(("Tasks"
;;                :keys "t"
;;                :file ,(concat cdom/org-agenda-directory "inbox.org")
;;                :prepend t
;;                :template "* %{todo-state} %^{Description}"
;;                :todo-state "TODO"))))

(use-package! deft
  :config
  (setq deft-directory "~/org"
        deft-recursive t))

(use-package! org-roam
  :init
  (setq org-roam-directory "~/org")
  :after (doct))
  ;; :config
  ;; (setq org-roam-dailies-capture-templates
  ;;       (doct `(("daily") :keys "d"
  ;;               :type plain
  ;;               :function org-roam-capture--get-point
  ;;               :template "%?"
  ;;               :unnarrowed t
  ;;               :immediate-finish t
  ;;               :file-name ,(concat cdom/org-agenda-directory "%<%Y-%m-%d>.org")
  ;;               :head "#+title: %<%A, %d %B %Y>")))
  ;; (setq +org-roam-open-buffer-on-find-file nil))

;; Configure org-journal for compatability with org-roam-dailies
(use-package! org-journal
  :init
  (setq org-journal-file-type 'monthly
        org-journal-file-format "%Y-%m.org"
        org-journal-dir cdom/org-agenda-directory
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))

(use-package! fish-mode
  :config
  (setq fish-indent-offset 2
        fish-enable-auto-indent t))

;; https://github.com/ema2159/centaur-tabs#my-personal-configuration
(use-package! centaur-tabs
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))


;; [BROKEN] Archive items to an archive sibling instead of a separate file
(setq org-archive-default-command 'org-archive-to-archive-sibling)

(setq +doom-quit-messages '("(setq nothing t everything 'permitted)"
                            "Hey! Hey, M-x listen!"
                            "How fast can you take your time, kid?"
                            "Sous les pavés, la plage!"))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Montgomery"
      user-mail-address "chris@cdom.io")

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

;; [BROKEN] Archive items to an archive sibling instead of a separate file
(setq org-archive-default-command 'org-archive-to-archive-sibling)

(setq +doom-quit-messages '("(setq nothing t everything 'permitted)"
                            "Hey! Hey, M-x listen!"
                            "How fast can you take your time, kid?"
                            "Sous les pavés, la plage!"))

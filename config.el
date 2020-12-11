;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

(setq! doom-font (font-spec :family "Iosevka" :size 18)
       doom-unicode-font (font-spec :family "Iosevka")
       doom-variable-pitch-font (font-spec :family "Iosevka Sparkle"))

;; Enable font ligatures in emacs-mac@27.
(mac-auto-operator-composition-mode t)

;; Reduce the size of text in Zen Mode.
(setq! +zen-text-scale 1)

;; @TODO disable this if the emacs wrapper application can handle it (see below)
;; @TODO defer?! but it needs to load quickly
;; (use-package! auto-dark-emacs
;;   :hook 'load-theme
;;   :custom
;;   (auto-dark-emacs/dark-theme 'doom-monokai-pro)
;;   (auto-dark-emacs/light-theme 'doom-plain))

(load-theme 'doom-monokai-pro t)

;; Change theme based on macOS light/dark mode.
(add-hook 'ns-system-appearance-change-functions
          #'(lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light (load-theme 'doom-one-light t))
                ('dark (load-theme 'doom-monokai-pro t)))))

(setq! org-directory              "~/org"
       +cdom/org-agenda-directory "~/org/gtd"
       org-capture-todo-file      "inbox.org"
       org-roam-directory         "~/org")

;; Simple settings.
;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq! undo-limit 80000000
       evil-want-fine-undo nil
       truncate-string-ellipsis "…")

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq! ns-right-alternate-modifier 'none)

;; Autosave
(setq! auto-save-default t
       auto-save-no-message t)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; https://tecosaur.github.io/emacs-config/config.html#windows
(setq! evil-vsplit-window-right t
       evil-split-window-below t)

;; Show previews in ivy.
(setq! +ivy-buffer-preview t)

;; List magit branches by date.
(setq! magit-list-refs-sortby "-creatordate")

;; Attempts to prevent vterm from loading emacs from within itself
(use-package! with-editor
  :after (vterm)
  :general
  ([remap async-shell-command] 'with-editor-async-shell-command)
  ([remap shell-command] 'with-editor-shell-command)
  :hook
  (shell-mode . with-editor-export-editor)
  (term-exec  . with-editor-export-editor)
  (eshell-mode . with-editor-export-editor)
  (vterm-mode . with-editor-export-editor))

;; Show gravatars in magit
(setq! magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(after! org
  (defun +cdom/org-archive-done-tasks ()
    "Archive all completed tasks in a file to an archive sibling."
    (interactive)
    (org-map-entries 'org-archive-to-archive-sibling "/DONE|KILL" 'file))
  (require 'find-lisp)
  (setq! org-agenda-files (find-lisp-find-files
                           +cdom/org-agenda-directory
                           "\.org$")
         org-archive-default-command 'org-archive-to-archive-sibling
         org-export-copy-to-kill-ring 'if-interactive
         org-log-refile 'time))

(use-package! doct
  :after (org)
  :commands (doct))

(use-package! org-roam
  :init
  :after (doct))

;; Add doct support to org-roam capture templates.
(after! doct org-roam
  (defun +doct-org-roam (groups)
    (let (converted)
      (dolist (group groups)
        (let* ((props (nthcdr 5 group))
               (roam-properties (plist-get (plist-get props :doct) :org-roam)))
          (push `(,@group ,@roam-properties) converted)))
      (setq! doct-templates (nreverse converted))))
  (setq! doct-after-conversion-functions '(+doct-org-roam)))
;; :config
;; (setq! org-roam-dailies-capture-templates
;;       (doct `(("daily") :keys "d"
;;               :type plain
;;               :function org-roam-capture--get-point
;;               :template "%?"
;;               :unnarrowed t
;;               :immediate-finish t
;;               :file-name ,(concat cdom/org-agenda-directory "%<%Y-%m-%d>.org")
;;               :head "#+title: %<%A, %d %B %Y>")))
;; (setq! +org-roam-open-buffer-on-find-file nil))

;; (setq! org-capture-templates
;;       (doct `(("Tasks"
;;                :keys "t"
;;                :file ,(concat cdom/org-agenda-directory "inbox.org")
;;                :prepend t
;;                :template "* %{todo-state} %^{Description}"
;;                :todo-state "TODO"))))


;; Configure org-journal for compatability with org-roam-dailies
(use-package! org-journal
  :defer-incrementally t
  :init
  (setq! org-journal-file-type 'monthly
         org-journal-file-format "%Y-%m.org"
         org-journal-dir +cdom/org-agenda-directory
         org-journal-date-format "%A, %d %B %Y"
         org-journal-enable-agenda-integration t))

(use-package! ox-gfm
  :after org)

(use-package! ox-jira
  :after org)

(use-package! vimrc-mode
  :defer-incrementally t
  :init
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))

(after! lsp
  :config
  (setq! lsp-vetur-format-default-formatter-js "prettier-eslint"
         lsp-vetur-format-default-formatter-ts "prettier-eslint"
         lsp-vetur-use-workspace-dependencies t))

(setq! +ligatures-extra-symbols
       '(
         ;; org
         :name          "»"
         :src_block     "»"
         :src_block_end "«"
         :quote         "“"
         :quote_end     "”"
         ;; Functional
         :lambda        "λ"
         :def           "ƒ"
         :composition   "∘"
         ;; :map           "↦"
         ;; Types
         :null          "∅"
         :true          "𝕋"
         :false         "𝔽"
         ;; :int           "ℤ"
         ;; :float         "ℝ"
         ;; :str           "𝕊"
         ;; :bool          "𝔹"
         ;; :list          "𝕃"
         ;; Flow
         ;; :not           "￢"
         :in            "∈"
         :not-in        "∉"
         :and           "∧"
         :or            "∨"
         ;; :for           "∀"
         ;; :some          "∃"
         :return        "⟼"
         :yield         "⟻"
         ;; Other
         :union         "⋃"
         :intersect     "∩"
         ;; :diff          "∖"
         ;; :tuple         "⨂"
         ;; :pipe          "" ;; FIXME: find a non-private char
         :dot           "•"))

(setq! +doom-quit-messages
       '("(setq nothing t everything 'permitted)"
         "Hey! Hey, M-x listen!"
         "How fast can you take your time, kid?"
         "Sous les pavés, la plage!"
         "You know how everyone's into weirdness right now?"
         "We have such sights to show you..."
         "Take a break."
         "Is Control controlled by its need to control?"))

(load! "~/.emacs.private")

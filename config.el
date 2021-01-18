;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

(setq! doom-font (font-spec :family "Iosevka" :size 14)
       doom-unicode-font (font-spec :family "Iosevka")
       doom-variable-pitch-font (font-spec :family "Iosevka Sparkle"))

;; Enable font ligatures in emacs-mac@27.
(if IS-MAC (mac-auto-operator-composition-mode t))

;; Start the emacs server.
;; Open a new frame with `emacsclient -cn'.
(server-start)

;; Reduce the size of text in Zen Mode.
(setq! +zen-text-scale 1)

(defun +cdom/os-theme (status)
  "Get the theme corresponding to the system's current dark mode status."
  (intern
    (pcase status
      ("dark" (getenv "CDOM_EMACS_THEME_DARK"))
      ("light" (getenv "CDOM_EMACS_THEME_LIGHT"))
      (_ "base16-black-metal-khold"))))

;; @TODO accept param to avoid needing to call `cdom-os-appearance' :performance:
(defun +cdom/load-os-theme ()
  "Load the theme corresponding to the system's dark mode status."
  (interactive)
  (load-theme (+cdom/os-theme (string-trim-right (shell-command-to-string "cdom-os-appearance"))) t))

(use-package! base16-theme
  :after-call +cdom/load-os-theme
  :config
  (setq! base16-theme-256-color-source "base16-shell"
         base16-distinct-fringe-background nil))

;; Load default theme based on macOS dark mode status.
(+cdom/load-os-theme)


(setq! org-directory "~/org"
       +cdom/org-agenda-directory "~/org/gtd/"
       org-capture-todo-file "inbox.org"
       org-roam-directory "~/org")

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
;; (setq! +ivy-buffer-preview t)

(after! magit
  ;; List magit branches by date.
  (setq! magit-list-refs-sortby "-creatordate")
  ;; Enable delta diff viewer
  (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))

;; Prevent evil-lion from removing extra spaces.
;; Add any desired extra space prior to invoking evil-lion.
;; (setq! evil-lion-squeeze-spaces nil)

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

(after! org
  (defun +cdom/org-archive-done-tasks ()
    "Archive all completed tasks in a file to an archive sibling."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE|KILL" 'file))
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

(use-package! org-board
  :defer t)

;; Add a CREATED property to org-mode headings.
(use-package! org-expiry
  :after (org)
  :config
  (setq! org-expiry-inactive-timestamps t))
;; (org-expiry-insinuate))

;; (use-package! org-protocol-capture-html
;;   :after (org))

(use-package! org-roam
  :after (doct))

(use-package! org-web-tools
  :after (org))

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
         "Is Control controlled by its need to control?"
         "Nothing here now but the recordings..."))

(load! "~/.emacs.private")

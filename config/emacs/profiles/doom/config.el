;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

(setq! doom-font (font-spec :family "PragmataPro Liga" :size 16)
       doom-big-font (font-spec :family "PragmataPro Liga" :size 28)
       doom-unicode-font (font-spec :family "PragmataPro Liga")
       doom-variable-pitch-font (font-spec :family "PragmataPro Liga"))

;; Start the emacs server.
;; Open a new frame with `emacsclient -cn'.
(server-start)

;; Display the fill-column indicator.
(global-display-fill-column-indicator-mode +1)

;; Reduce the size of text in Zen Mode.
;; (setq! +zen-text-scale 1)


;; === modeline =====================

(use-package! moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function)
  (custom-set-faces!
    '((mode-line mode-line-inactive)
      :family "PragmataPro Mono"
      :size 10)))

(use-package! minions
  :config (minions-mode 1))


;; Adjust the size of the modeline.
;; (after! doom-modeline
;;   (when IS-MAC
;;     (setq! doom-modeline-height 1)
;;     (custom-set-faces!
;;       '((mode-line mode-line-inactive) :family "PragmataPro Mono" :size 12))))

(setq! tab-width 2)

(use-package! evil
  :config
  (setq! evil-shift-width 2
         evil-vsplit-window-right t))

(defun +cdom/load-os-theme ()
  "Load the theme corresponding to the system's dark mode status."
  (interactive)
  (let ((status (string-trim-right (shell-command-to-string "dotfield_os_appearance"))))
    (if (equal "light" status)
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi))))

(use-package! modus-themes
  :init
  (require 'modus-themes)

  (setq
   ;; meta
   modus-themes-inhibit-reload nil

   ;; type
   modus-themes-bold-constructs nil
   modus-themes-italic-constructs t
   modus-themes-mixed-fonts t

   ;; ui
   modus-themes-completions nil
   modus-themes-fringes nil
   modus-themes-hl-line '(accented)
   modus-themes-links '(neutral-underline)
   modus-themes-mode-line '(moody borderless)
   modus-themes-tabs-accented nil

   ;; syntax
   modus-themes-paren-match 'bold
   modus-themes-syntax '(alt-syntax)
   modus-themes-headings '((1 . (background bold overline))
                           (2 . (bold rainbow))
                           (3 . (bold))
                           (4 . (no-bold))
                           (5 . (no-bold))
                           (6 . (no-bold))))
  ;; Required upon initial load
  (modus-themes-load-themes)

  :config
  ;; Load theme based on macOS dark mode status.
  (+cdom/load-os-theme))

(defvar +cdom/org-agenda-directory "~/org/gtd/")
(defvar +cdom/org-notes-directory "~/org/notes/")
(defvar +cdom/org-mind-directory "~/org/mind/")

(setq! org-directory "~/org"
       +org-capture-todo-file (concat +cdom/org-agenda-directory "inbox.org")
       org-roam-directory org-directory
       deft-directory org-directory
       deft-recursive t)

;; Store the value of the shell environment's =SSH_*= variables when generating
;; the env file.
(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))

(appendq! safe-local-eval-forms '((sh-set-shell "sh")
                                  (sh-set-shell "bash")
                                  (sh-set-shell "zsh")))

;; Simple settings.
;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq! undo-limit 80000000
       truncate-string-ellipsis "…"
       display-line-numbers-type 'relative)

;; Change default buffer and frame names.
;; https://tecosaur.github.io/emacs-config/config.html#window-title
(setq! doom-fallback-buffer-name "► Doom"
       +doom-dashboard-name "► Doom"
       frame-title-format
       '(""
         (:eval
          (if (s-contains-p org-roam-directory (or buffer-file-name ""))
              (replace-regexp-in-string
               ".*/[0-9]*-?" "☰ "
               (subst-char-in-string ?_ ?  buffer-file-name))
            "%b"))
         (:eval " ▲ doom")
         (:eval
          (when (frame-parent) " ◂ [child]"))))

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq! ns-right-alternate-modifier 'none)

;; Autosave
(setq! auto-save-default t
       auto-save-no-message t)
;; TODO: This still throws a message because it's called on the hook, unaffected
;; by ~auto-save-no-message~
;;
;; TODO: may be causing crashes when performing other actions simultaneously?
;; not just limited to actions in org files fwiw.
;; (add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; https://gitlab.com/ideasman42/emacs-scroll-on-jump
;; (use-package! scroll-on-jump
;;   :after (evil)
;;   :config
;;   (setq! scroll-on-jump-duration 0.2
;;          scroll-on-jump-smooth t
;;          scroll-on-jump-use-curve nil)
;;   (scroll-on-jump-advice-add evil-undo)
;;   (scroll-on-jump-advice-add evil-redo)
;;   (scroll-on-jump-advice-add evil-jump-item)
;;   (scroll-on-jump-advice-add evil-jump-forward)
;;   (scroll-on-jump-advice-add evil-jump-backward)
;;   (scroll-on-jump-advice-add evil-ex-search-next)
;;   (scroll-on-jump-advice-add evil-ex-search-previous)
;;   (scroll-on-jump-advice-add evil-forward-paragraph)
;;   (scroll-on-jump-advice-add evil-backward-paragraph)
;;   ;; Actions that themselves scroll.
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
;;   (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

;; https://tecosaur.github.io/emacs-config/config.html#company
;; (after! company
;;   (setq! company-idle-delay nil)
;;   ;; Make aborting less annoying.
;;   (add-hook 'evil-normal-state-entry-hook #'company-abort))

(use-package! company-box
  :config
  ;; Disable the documentation childframe because it causes emacs to crash!
  ;;
  ;; FIXME Allow doc childframe flyout without crashing
  ;;
  ;; Note that Emacs doesn't crash when running Doom+modules without my config...
  (setq! company-box-doc-enable nil))

;; Extend prescient history lifespan.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; (use-package! which-key
;;   :init
;;   (setq! which-key-sort-order
;;          ;; default
;;          ;; 'which-key-key-order
;;          ;; sort based on the key description ignoring case
;;          ;; 'which-key-description-order
;;          ;; same as default, except single characters are sorted alphabetically
;;          ;; 'which-key-key-order-alpha
;;          ;; same as default, except all prefix keys are grouped together at the end
;;          ;; 'which-key-prefix-then-key-order
;;          ;; same as default, except all keys from local maps shown first
;;          'which-key-local-then-key-order))

;; (setq! which-key-allow-multiple-replacements t)
;; (after! which-key
;;   ;; Remove ~evil-~ prefix from keybinding labels
;;   ;; https://tecosaur.github.io/emacs-config/config.html#which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(after! magit
  ;; List magit branches by date.
  (setq! magit-list-refs-sortby "-creatordate"
         magit-process-finish-apply-ansi-colors t))

;; Enable delta diff viewer
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))

;; Prevent evil-lion from removing extra spaces.
;; Add any desired extra space prior to invoking evil-lion.
;; (setq! evil-lion-squeeze-spaces nil)

;; Prevent vterm from loading emacs from within itself
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

;; https://tecosaur.github.io/emacs-config/config.html#tweaking-defaults
(use-package! org
  :config
  (setq! org-image-actual-width 300
         ;; org-startup-folded t
         org-startup-with-inline-images t
         org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
         org-cycle-separator-lines -1
         ;; TODO: could this cause an issue with org-roam IDs?
         org-use-property-inheritance t              ; it's convenient to have properties inherited
         org-log-done 'time                          ; log the time an item was completed
         org-log-refile 'time
         org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
         org-catch-invisible-edits 'smart          ; try not to accidently do weird stuff in invisible regions
         org-export-copy-to-kill-ring 'if-interactive)
  (defun +cdom/org-archive-done-tasks ()
    "Archive all completed tasks in a file to an archive sibling."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE|KILL" 'file))
  (require 'find-lisp)
  (setq! org-agenda-files (find-lisp-find-files
                           +cdom/org-agenda-directory
                           "\.org$")
         org-archive-default-command 'org-archive-to-archive-sibling))

(use-package! doct
  :after (org-capture)
  :commands (doct))

(after! js2-mode
  ;; Use eslintd for faster ESLint-based formatting on save.
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (set-company-backend! 'company-tide 'js2-mode))

(after! sh-script
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet)))

(use-package! org-board
  :defer t)

(use-package! devdocs-browser
  :defer t)

;; Add a CREATED property to org-mode headings.
;; (use-package! org-expiry
;;   :after (org)
;;   :config
;;   (setq! org-expiry-inactive-timestamps t)
;;   (org-expiry-insinuate))

;; (use-package! org-protocol-capture-html
;;   :after (org))

(use-package! org-web-tools
  :after (org))

(after! org-capture
  (defun set-org-capture-templates ()
    (setq! org-capture-templates
           (doct `(("Personal todo"
                    :keys "t"
                    :icon ("checklist" :set "octicon" :color "green")
                    :file +org-capture-todo-file
                    :prepend t
                    :headline "Inbox"
                    :type entry
                    :template ("* TODO %?"
                               "%i %a"))))))
  (set-org-capture-templates))

;; Make deft understand files created by org-roam
;; https://github.com/jrblevin/deft/issues/75#issuecomment-905031872
(defun cdom/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))
(advice-add 'deft-parse-title :override #'cdom/deft-parse-title)
(setq! deft-strip-summary-regexp
       (concat "\\("
               "[\n\t]" ;; blank
               "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
               "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
               "\\)"))

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

(after! markdown
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode)))

(use-package! org-jira
  :init
  (setq! jiralib-url "https://alleyinteractive.atlassian.net"
         org-jira-working-dir (concat (getenv "XDG_DATA_HOME") "/emacs/org-jira"))
  (make-directory org-jira-working-dir 'parents))

(use-package! vimrc-mode
  :defer-incrementally t
  :init
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package! web-mode
  :config
  ;; Prevent web-mode from loading for all PHP files in WordPress themes.
  ;; Overrides doom behavior.
  (add-to-list 'auto-mode-alist '("wp-content/themes/.+\\.php\\'" . php-mode))
  ;; Template partials should still load web-mode.
  (add-to-list 'auto-mode-alist '("wp-content/.+/template-parts/.+\\.php\\'" . web-mode)))

(use-package! nixpkgs-fmt
  :config
  ;; Use nixpkgs-fmt instead of nixfmt
  (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode))

(use-package! projectile
  :config
  (appendq! projectile-globally-ignored-directories '("client-mu-plugins/vendor")))

(after! projectile
  (setq! doom-projectile-cache-purge-non-projects t))

(use-package! treemacs
  :config
  (setq! +treemacs-git-mode 'deferred
         treemacs-tag-follow-mode t))

(use-package! lsp-mode
  :config
  (setq! lsp-vetur-use-workspace-dependencies t
         lsp-enable-indentation t
         ;; TODO: might need to load after
         lsp-ui-doc-delay 2
         flycheck-javascript-eslint-executable "eslint_d")

  ;; Sync LSP workspace folders and treemacs projects.
  (lsp-treemacs-sync-mode 1)

  ;; Register rnix-lsp as a client
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))

  ;; Add multi-root workspace folders on demand.
  ;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
  ;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

  ;; `lsp-mode' integration with Flycheck `sh-shellcheck' checker
  ;; https://old.reddit.com/r/emacs/comments/hqxm5v/weekly_tipstricketc_thread/fy4pvr8/?context=3
  (defun +cdom--lsp-flycheck-enable-shellcheck ()
    "Enable Shellcheck for shell buffers under LSP."
    (when (derived-mode-p 'sh-mode)
      (flycheck-add-next-checker 'lsp 'sh-shellcheck)))
  (add-hook 'lsp-after-open-hook #'+cdom--lsp-flycheck-enable-shellcheck))

(use-package! literate-calc-mode
  :defer-incrementally t)

(set-ligatures! 'org-mode
  :todo "TODO")

(after! mu4e
  (setq! sendmail-program (executable-find "msmtp")
         send-mail-function #'smtpmail-send-it
         message-sendmail-f-is-evil t
         message-sendmail-extra-arguments '("--read-envelope-from")
         message-send-mail-function #'message-send-mail-with-sendmail))

(setq! mu4e-context-policy 'ask-if-none
       mu4e-compose-context-policy 'always-ask
       +mu4e-gmail-accounts '(("chris@cdom.io" . "personal")
                              ("chris@alley.co" . "work")))

(set-email-account! "personal"
                    '((mu4e-sent-folder       . "/personal/sent")
                      (mu4e-drafts-folder     . "/personal/drafts")
                      (mu4e-trash-folder      . "/personal/trash")
                      (mu4e-refile-folder     . "/personal/archive")
                      (smtpmail-smtp-user     . "chris@cdom.io")
                      (mu4e-compose-signature . "---\nChris Montgomery\nSenior Software Developer\nAlley"))
                    t)
(set-email-account! "work"
                    '((mu4e-sent-folder       . "/work/sent")
                      (mu4e-drafts-folder     . "/work/drafts")
                      (mu4e-trash-folder      . "/work/trash")
                      (mu4e-refile-folder     . "/work/archive")
                      (smtpmail-smtp-user     . "chris@alley.co")
                      (mu4e-compose-signature . "---\nChris Montgomery\nSenior Software Developer\nAlley"))
                    t)

(plist-put! +ligatures-extra-symbols
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
            :map           "↦"

            ;; Types
            :null          "∅"
            :true          "𝕋"
            :false         "𝔽"
            :int           "ℤ"
            :float         "ℝ"
            :str           "𝕊"
            :bool          "𝔹"
            :list          "𝕃"

            ;; Flow
            :not           "!"
            :in            "∈"
            :not-in        "∉"
            :and           "∧"
            :or            "∨"
            :for           "∀"
            :some          "∃"
            :return        "⟼"
            :yield         "⟻"

            ;; Other
            :union         "⋃"
            :intersect     "∩"
            :diff          "∖"
            :tuple         "⨂"
            :pipe          " "
            :dot           "•")

(setq! +doom-quit-messages
       '("(setq nothing t everything 'permitted)"
         "Hey! Hey, M-x listen!"
         "How fast can you take your time, kid?"
         "Sous les pavés, la plage!"
         "You know how everyone's into weirdness right now?"
         "We have such sights to show you..."
         "Take a break."
         "Is Control controlled by its need to control?"
         "Nothing here now but the recordings..."
         "A silver light pops in your eyes..."
         "I have nothing to say, and I am saying it."
         "Who walkies the walkmen?"
         "I'm making my lunch!"
         "Eat protein!"))

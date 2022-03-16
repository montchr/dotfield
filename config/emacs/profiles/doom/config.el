;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; === basic ===================================================================

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

(setq! doom-font (font-spec :family "PragmataPro Liga" :size 16)
       doom-big-font (font-spec :family "PragmataPro Liga" :size 28)
       doom-unicode-font (font-spec :family "PragmataPro Liga")
       doom-variable-pitch-font (font-spec :family "PragmataPro Liga"))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
;; (setq gc-cons-threshold 100000000)

;; Start the emacs server.
;; Open a new frame with `emacsclient -cn'.
(server-start)

;; Display the fill-column indicator.
(global-display-fill-column-indicator-mode +1)

;; Simple settings.
;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq! undo-limit 80000000
       truncate-string-ellipsis "…"
       display-line-numbers-type 'relative)

(setq! tab-width 2)

(use-package! evil
  :config
  (setq! evil-shift-width 2
         evil-vsplit-window-right t))

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq! ns-right-alternate-modifier 'none)

;; Extend prescient history lifespan.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Prevent evil-lion from removing extra spaces.
;; Add any desired extra space prior to invoking evil-lion.
;; (setq! evil-lion-squeeze-spaces nil)


;; === buffers =================================================================

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

;; Autosave
(setq! auto-save-default t
       auto-save-no-message t)
;; TODO: This still throws a message because it's called on the hook, unaffected
;; by ~auto-save-no-message~
;;
;; TODO: may be causing crashes when performing other actions simultaneously?
;; not just limited to actions in org files fwiw.
;; (add-hook 'auto-save-hook 'org-save-all-org-buffers)


;; === env =====================================================================

;; Store the value of the shell environment's =SSH_*= variables when generating
;; the env file.
(when noninteractive
  (add-to-list 'doom-env-whitelist "^SSH_"))


;; === modeline ================================================================

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


;; === appearance ==============================================================

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
   modus-themes-syntax '(alt-syntax))

  ;; Required upon initial load
  (modus-themes-load-themes)

  :config
  ;; Load theme based on macOS dark mode status.
  (+cdom/load-os-theme))

;; Reduce the size of text in Zen Mode.
;; (setq! +zen-text-scale 1)

;; https://github.com/konrad1977/emacs/blob/main/init.el
(use-package! svg-tag-mode
  :hook ((prog-mode . svg-tag-mode)
         (org-mode . svg-tag-mode))
  :config
  (setq svg-tag-tags
        '(
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
          ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
          ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))
          
          ("\\/\\/\\W?eslint-disable" . ((lambda (tag) (svg-tag-make "eslint-disable" :face 'org-level-3 :inverse t :margin 0 :crop-right t))))
          ("eslint-disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-3 :crop-left t))))
          
          ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
          )))


;; === agenda ==================================================================

(setq! org-directory "~/Dropbox/org"
       +org-capture-todo-file (concat org-directory "inbox.org")
       org-roam-directory org-directory
       deft-directory org-directory)

(after! org-capture
  (setq!
   org-capture-templates
   (doct `(
           ("Personal todo"
            :keys "t"
            :icon ("checklist" :set "octicon" :color "green")
            :file +org-capture-todo-file
            :prepend t
            :headline "Inbox"
            :type entry
            :template ("* TODO %?"
                       "%i %a"))
           ))))

(after! org-agenda
  ;; Hide all tags from agenda.
  (setq! org-agenda-hide-tags-regexp "."
         org-agenda-prefix-format
         '((agenda . " %i %-12:c%?-12t% s")
           (todo   . " ")
           (tags   . " %i %-12:c")
           (search . " %i %-12:c"))))

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
  (setq! org-archive-default-command 'org-archive-to-archive-sibling))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(use-package! doct
  :after (org-capture)
  :commands (doct))

;; Add a CREATED property to org-mode headings.
;; (use-package! org-expiry
;;   :after (org)
;;   :config
;;   (setq! org-expiry-inactive-timestamps t)
;;   (org-expiry-insinuate))

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
(after! org-journal
  (setq! org-journal-file-type 'daily
         org-journal-file-format "%Y-%m-%d.org"
         org-journal-dir org-directory
         org-journal-date-format "%A, %d %B %Y"
         org-journal-enable-agenda-integration t))

;; === company =================================================================

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


;; === projects ================================================================

(after! magit
  ;; List magit branches by date.
  (setq! magit-list-refs-sortby "-creatordate"
         magit-process-finish-apply-ansi-colors t))

(use-package! projectile
  :config
  (appendq! projectile-globally-ignored-directories '("client-mu-plugins/vendor")))

(after! projectile
  (setq! doom-projectile-cache-purge-non-projects t))

;; === languages ===============================================================

;; (after! js2-mode
;;   (set-company-backend! 'company-tide 'js2-mode))

  ;; Use eslintd for faster ESLint-based formatting on save.
(set-formatter! 'eslintd 'eslintd-fix :modes '(js2-mode))

;; (after! sh-script
;;   (set-company-backend! 'sh-mode
;;     '(company-shell :with company-yasnippet)))

(after! markdown
  (add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode)))

;; (appendq! safe-local-eval-forms '((sh-set-shell "sh")
;;                                   (sh-set-shell "bash")
;;                                   (sh-set-shell "zsh")))

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

;; Nix formatting with Alejandra
(set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))
(setq-hook! 'nix-mode-hook +format-with 'alejandra)

(use-package! lsp-mode
  :init
  (setq! lsp-use-plists t)

  :config
  ;; Register rnix-lsp as a client
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(after! lsp-mode
  (setq! lsp-vetur-use-workspace-dependencies t
         lsp-enable-indentation nil
         lsp-file-watch-threshold 666
         lsp-ui-doc-delay 2
         flycheck-javascript-eslint-executable "eslint_d"))


;; === tools ===================================================================

(use-package! org-board
  :defer t)

(use-package! devdocs-browser
  :defer t)

;; (use-package! org-protocol-capture-html
;;   :after (org))

(use-package! org-web-tools
  :after (org))

(use-package! ox-gfm
  :after org)

(use-package! ox-jira
  :after org)

(use-package! literate-calc-mode
  :defer-incrementally t)

;; === mail ====================================================================

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


;; === misc. ===================================================================

(set-ligatures! 'org-mode
  :todo "TODO")

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

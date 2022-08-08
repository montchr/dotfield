;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; === basic ===================================================================

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

;; Display the fill-column indicator.
(global-display-fill-column-indicator-mode +1)

;; Simple settings.
;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq! display-line-numbers-type 'relative)

(setq! tab-width 2)

(use-package! evil
  :config
  (setq! evil-shift-width 2
         evil-vsplit-window-right t))

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq! ns-right-alternate-modifier 'none)

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


;; === completion =============================================================

(use-package! embark-vc
  :after embark)


;; === modeline ================================================================


;; === appearance ==============================================================

(defun +cdom/load-os-theme ()
  "Load the theme corresponding to the system's dark mode status."
  (interactive)
  (let ((status (string-trim-right (shell-command-to-string "dotfield_os_appearance"))))
    (if (equal "light" status)
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi))))

(use-package! fontaine
  :commands (fontaine-store-latest-preset)
  :hook (kill-emacs-hook fontaine-store-latest-preset)
  :config
  (setq fontaine-presets
        '((small
           :default-height 100)
          (regular
           :default-height 120)
          (large
           :default-height 150)
          (t
           :default-family "Iosevka Xtal"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil
           :fixed-pitch-family nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :variable-pitch-family "IBM Plex Sans"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  ;; https://github.com/mickeynp/ligature.el/wiki#iosevka
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it per
  ;; mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package! modus-themes
  :init
  (setq!
   ;; meta
   modus-themes-inhibit-reload nil

   ;; type
   modus-themes-italic-constructs t

   ;; ui
   ;; modus-themes-completions nil
   ;; modus-themes-fringes nil
   modus-themes-hl-line '(accented)
   modus-themes-links '(background neutral-underline)
   modus-themes-mode-line '(borderless)
   modus-themes-tabs-accented nil
   modus-themes-box-buttons '(accented variable-pitch)

   ;; syntax
   ;; modus-themes-syntax '(alt-syntax)
   modus-themes-markup '(background)
   modus-themes-org-blocks '(gray-background)
   modus-themes-paren-match '(bold))

  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; Reduce the size of text in Zen Mode.
;; (setq! +zen-text-scale 1)

;; TODO: these don't work quite right
;; https://github.com/konrad1977/emacs/blob/main/init.el
;; (use-package! svg-tag-mode
;;   :hook ((prog-mode . svg-tag-mode)
;;          (org-mode . svg-tag-mode))
;;   :config
;;   (setq svg-tag-tags
;;         '(
;;           ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
;;           ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
;;           ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
;;           ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

;;           ("\\/\\/\\W?eslint-disable" . ((lambda (tag) (svg-tag-make "eslint-disable" :face 'org-level-3 :inverse t :margin 0 :crop-right t))))
;;           ("eslint-disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-3 :crop-left t))))

;;           ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
;;           ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
;;           )))


;; === org-mode ================================================================

(setq! org-directory "~/Documents/notes"
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
         ;; org-log-refile 'time
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
  ;; FIXME: malformed function?
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


;; === projects ================================================================

(after! magit
  ;; List magit branches by date.
  (setq! magit-list-refs-sortby "-creatordate"
         magit-process-finish-apply-ansi-colors t))

(use-package! projectile
  :config
  (appendq! projectile-globally-ignored-directories '("vendor")))

(after! projectile
  (setq! doom-projectile-cache-purge-non-projects t))


;; === ide =====================================================================

(use-package! format-all
  :custom
  ;; N.B. This is a subset of available languages -- only the ones I might use.
  ((format-all-default-formatters
    '(("C" clang-format)
      ("C#" clang-format)
      ("C++" clang-format)
      ("CMake" cmake-format)
      ("CSS" prettier)
      ("Dhall" dhall)
      ("Dockerfile" dockfmt)
      ("Emacs Lisp" emacs-lisp)
      ("Fish" fish-indent)
      ("GraphQL" prettier)
      ("Haskell" brittany)
      ("HTML" html-tidy)
      ("JavaScript" prettier)
      ("JSON" prettier)
      ("JSX" prettier)
      ("Less" prettier)
      ("Lua" lua-fmt)
      ("Markdown" prettier)
      ("Nix" alejandra)
      ("Perl" perltidy)
      ;; N.B. usually php projects use some lang-specific tool for this, but i
      ;; can also see why it could make sense to use prettier. either way, as of
      ;; this writing, format-all only supports prettier for php.
      ("PHP" prettier)
      ("Python" black)
      ("Ruby" rufo)
      ("SCSS" prettier)
      ("Shell" shfmt)
      ("SQL" sqlformat)
      ("Terraform" terraform-fmt)
      ("TOML" prettier)
      ("TSX" prettier)
      ("TypeScript" prettier)
      ("XML" html-tidy)
      ("YAML" prettier)
      ("_Ledger" ledger-mode)
      ("_Nginx" nginxfmt))))

  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode))

(use-package! lsp-mode
  :init
  (setq! lsp-use-plists t)
  :config
  (setq! lsp-vetur-use-workspace-dependencies t
         lsp-enable-indentation nil
         lsp-file-watch-threshold 666
         lsp-ui-doc-delay 2))


;; === languages ===============================================================

(use-package! apheleia)

(after! markdown
  (add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode)))

(use-package! vimrc-mode
  :defer-incrementally t
  :init
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))


;;; --- nix ----------------------------

;; Register rnix-lsp as a client
(use-package! lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))


;;; --- php ----------------------------
;;; sources:
;; - https://github.com/elken/doom#php

(use-package! web-mode
  :config
  ;; Prevent web-mode from loading for all PHP files in WordPress themes.
  ;; Overrides doom behavior.
  (add-to-list 'auto-mode-alist '("wp-content/themes/.+\\.php\\'" . php-mode))
  ;; Template partials should still load web-mode.
  (add-to-list 'auto-mode-alist '("wp-content/.+/template-parts/.+\\.php\\'" . web-mode)))

(after! eglot
  (add-hook 'php-mode-hook 'eglot-ensure)
  (defclass eglot-php (eglot-lsp-server) () :documentation "PHP's Intelephense")
  (cl-defmethod eglot-initialization-options ((server eglot-php))
    "Passes through required intelephense options"
    `(:storagePath ,php-intelephense-storage-path
      :licenceKey ,lsp-intelephense-licence-key
      :clearCache t))
  (add-to-list 'eglot-server-programs `((php-mode phps-mode) . (eglot-php . (,php-intelephense-command "--stdio")))))

;; FIXME: needed for lsp-mode too?
(when (featurep! :tools lsp +eglot)
  (defvar php-intelephense-storage-path (expand-file-name "lsp-intelephense" doom-etc-dir))
  (defvar php-intelephense-command (expand-file-name "lsp/npm/intelephense/bin/intelephense" doom-etc-dir)))

(after! (:or lsp-mode eglot)
  (setq! lsp-intelephense-licence-key (or (ignore-errors (fetch-auth-source :user "intelephense") nil))
         ;; Enable WordPress stubs.
         ;; FIXME: append only -- but this format has been difficult to work with
         lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar"
                                 "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant"
                                 "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase"
                                 "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli"
                                 "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql"
                                 "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode"
                                 "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets"
                                 "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg"
                                 "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "wordpress" "xml" "xmlreader" "xmlrpc"
                                 "xmlwriter" "Zend OPcache" "zip" "zlib"]))


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
       mu4e-compose-context-policy 'always-ask)

(set-email-account! "personal"
                    '((mu4e-sent-folder       . "/personal/sent")
                      (mu4e-drafts-folder     . "/personal/drafts")
                      (mu4e-trash-folder      . "/personal/trash")
                      (mu4e-refile-folder     . "/personal/archive")
                      (smtpmail-smtp-user     . "chris@cdom.io"))
                    t)


;; === misc. ===================================================================

(setq! +doom-quit-messages
       '("(setq nothing t everything 'permitted)"
         "Hey! Hey, M-x listen!"
         "How fast can you take your time, kid?"
         "Sous les pavés, la plage!"
         "You know how everyone's into weirdness right now?"
         "We have such sights to show you..."
         "Take a break."
         "Is Control controlled by its need to control?"
         "Wise words from the departed: Eat your greens. Especially broccoli."
         "Remember to say 'thank you' for the things you never had."
         "Nothing here now but the recordings..."
         "I have nothing to say, and I am saying it."
         "Who walkies the walkmen?"
         "The Empire never ended."
         "Chaos never died."
         "There is no done."
         "Thesis -> Antithesis -> Synthesis"
         "I'm making my lunch!"
         "Eat protein!"))

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

(setq enable-dir-local-variables t)

;; Allow the default macOS ~alt~ behavior for special keyboard chars.
(setq! ns-right-alternate-modifier 'none)

;; camelCaseWords
(global-subword-mode 1)

;; Globally s/
(setq evil-ex-substitute-global t)

;; === modeline ================================================================

(after! doom-modeline
  (setq doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)))


;; === theme ===================================================================

(use-package! modus-themes
  :init
  (setq
   ;; --- type ---
   modus-themes-bold-constructs t
   modus-themes-italic-constructs t

   ;; --- ui ---
   modus-themes-completions nil
   ;; modus-themes-fringes nil
   modus-themes-hl-line '(accented)
   modus-themes-links '(background neutral-underline)
   modus-themes-mode-line '(borderless)
   modus-themes-tabs-accented nil
   modus-themes-box-buttons '(accented)

   ;; --- syntax ---
   modus-themes-syntax '(alt-syntax)
   modus-themes-markup '(background)
   modus-themes-org-blocks '(gray-background)
   modus-themes-paren-match '(bold))

  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))


;; === font ====================================================================

(use-package! ligature
  :after modus-themes
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

(use-package! fontaine
  :after ligature
  :config
  (setq fontaine-presets
        '((small :default-height 106
                 :bold-weight normal)
          (regular :default-height 124)
          (medium :default-height 133)
          (large :default-height 150
                 :bold-weight bold)
          (xlarge :default-height 170
                  :bold-weight bold)
          (t
           :default-family "Iosevka Xtal"
           :default-weight regular
           :default-height 124
           :fixed-pitch-family nil
           :fixed-pitch-family nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :variable-pitch-family "IBM Plex Sans"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil
            ;; I've often gotten the sense that bold constructs don't work so well
            ;; with a narrow programming font like Iosevka. It tends to result in
            ;; muddy-looking characters at small sizes.
           :bold-weight semibold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))


;; === completions =============================================================

(when (modulep! :completion company)
  (use-package! company-prescient
    :after company
    :hook (company-mode . company-prescient-mode)
    :hook (company-prescient-mode . prescient-persist-mode)
    :config
    (setq prescient-save-file (concat doom-cache-dir "prescient-save.el")
          history-length 1000)))

(set-company-backend! 'emacs-lisp-mode
                      'company-capf
                      'company-yasnippet)


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
         org-startup-with-inline-images t
         org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
         org-cycle-separator-lines -1
         org-use-property-inheritance t
         org-log-done 'time
         ;; org-log-refile 'time
         org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
         org-catch-invisible-edits 'smart
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




;; === projects ================================================================

(use-package! embark-vc
  :after embark)

(after! magit
  ;; List magit branches by date.
  (setq! magit-list-refs-sortby "-creatordate"
         magit-process-finish-apply-ansi-colors t))

(use-package! projectile
  :config
  (setq! doom-projectile-cache-purge-non-projects t)
  (setq! projectile-project-search-path
         '(("~/Developer/contrib/" . 2)
           ("~/Developer/sources/" . 1)
           ("~/Developer/work/" . 2))))


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
  (add-hook 'nix-mode-hook 'format-all-mode))

(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-progress-via-spinner t
        lsp-enable-file-watchers nil
        lsp-idle-delay 0.47
        lsp-completion-enable-additional-text-edit t
        lsp-signature-render-documentation t
        lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion)
        lsp-signature-doc-lines 10
        lsp-eldoc-enable-hover t
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-vetur-use-workspace-dependencies t
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  (when (modulep! :completion company)
    (setq +lsp-company-backends '(company-capf :with company-yasnippet company-dabbrev))))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
    lsp-ui-doc-position 'top
    lsp-ui-doc-delay 0.51
    lsp-ui-doc-max-width 50
    lsp-ui-doc-max-height 30
    lsp-ui-doc-include-signature t
    lsp-ui-doc-header t))


;; === languages ===============================================================

(after! markdown
  (add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode)))

(use-package! vimrc-mode
  :defer-incrementally t
  :init
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))

;;; --- nix ----------------------------

(after! lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "nil")
                     :major-modes '(nix-mode)
                     :server-id 'nix-nil-lsp)))

(after! eglot
  ;; https://github.com/oxalica/nil
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))


;;; --- php ----------------------------
;;; sources:
;; - https://github.com/elken/doom#php

(defvar xref-ignored-files '("_ide_helper_models.php" "_ide_helper.php")
  "List of files to be ignored by `xref'.")

(defun xref-ignored-file-p (item)
  "Return t if `item' should be ignored."
  (seq-some
   (lambda (cand)
     (string-suffix-p cand (oref (xref-item-location item) file))) xref-ignored-files))

(defadvice! +lsp--ignored-locations-to-xref-items-a (items)
  "Remove ignored files from list of xref-items."
  :filter-return #'lsp--locations-to-xref-items
  (cl-remove-if #'xref-ignored-file-p items))

(defadvice! +lsp-ui-peek--ignored-locations-a (items)
  "Remove ignored files from list of xref-items."
  :filter-return #'lsp-ui-peek--get-references
  (cl-remove-if #'xref-ignored-file-p items))

(defvar php-intelephense-storage-path (expand-file-name "lsp-intelephense" doom-data-dir))
(defvar php-intelephense-command (expand-file-name "lsp/npm/intelephense/bin/intelephense" doom-data-dir))

(after! web-mode
  (pushnew! web-mode-engines-alist '(("blade"  . "\\.blade\\.")))
  ;; Prevent web-mode from loading for all PHP files in WordPress themes.
  ;; Overrides doom behavior.
  (add-to-list 'auto-mode-alist '("wp-content/themes/.+\\.php\\'" . php-mode))
  ;; Template partials should still load web-mode.
  (add-to-list 'auto-mode-alist '("wp-content/.+/template-parts/.+\\.php\\'" . web-mode)))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor")
  (setq +lsp-company-backends
        '(:separate company-capf company-yasnippet company-dabbrev)))

(after! (:or lsp-mode eglot)
  (setq! lsp-intelephense-licence-key (or (ignore-errors (fetch-auth-source :user "intelephense") nil))
         ;; Enable WordPress stubs.
         ;; TODO: append only -- but this format has been difficult to work with
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

(after! eglot
  (add-hook 'php-mode-hook 'eglot-ensure)
  (defclass eglot-php (eglot-lsp-server) () :documentation "PHP's Intelephense")
  (cl-defmethod eglot-initialization-options ((server eglot-php))
    "Passes through required intelephense options"
    `(:storagePath ,php-intelephense-storage-path
      :licenceKey ,lsp-intelephense-licence-key
      :clearCache t))
  (add-to-list 'eglot-server-programs `((php-mode phps-mode) . (eglot-php . (,php-intelephense-command "--stdio")))))


;; === snippets ===================================================================

(use-package! cape-yasnippet
  :after cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  (after! lsp-mode
    (add-hook 'lsp-managed-mode-hook #'cape-yasnippet--lsp))
  (after! eglot
    (add-hook 'eglot-managed-mode-hook #'cape-yasnippet--eglot)))


;; === tools ===================================================================

(use-package spdx
  :defer t
  :custom
  (spdx-copyright-holder 'auto)
  (spdx-project-detection 'auto))

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
         "Thesis -> Antithesis -> Synthesis"
         "I'm making my lunch!"
         "Eat protein!"))

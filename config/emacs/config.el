;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq! user-full-name "Chris Montgomery"
       user-mail-address "chris@cdom.io")

(setq! doom-font (font-spec :family "PragmataPro Liga" :size 14)
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

;; Adjust the size of the modeline.
(after! doom-modeline
  (when IS-MAC
    (setq! doom-modeline-height 1)
    (custom-set-faces!
      '((mode-line mode-line-inactive) :family "PragmataPro Mono" :size 12))))

;; Hide 'UTF-8' encoding from the modeline, since it's the default.
;; @TODO doesn't appear to be working. perhaps needs to be after doom-modeline?
;; https://tecosaur.github.io/emacs-config/config.html
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq! tab-width 2)

(use-package! evil
  :config
  (setq! evil-shift-width 2
         evil-vsplit-window-right t))

;; TODO: color theme inherits values from shell which can cause, for example, pointer to be the same color as background
(defun +cdom/load-os-theme ()
  "Load the theme corresponding to the system's dark mode status."
  (interactive)
  (let ((status (string-trim-right (shell-command-to-string "cdom_os_appearance"))))
    (if (equal "light" status)
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi))))

(use-package! modus-themes
  :init
  (require 'modus-themes)
  (setq! modus-themes-bold-constructs nil
         modus-themes-fringes 'subtle
         modus-themes-slanted-constructs t
         modus-themes-syntax '(faint yellow-comments green-string)
         modus-themes-no-mixed-fonts t
         modus-themes-mode-line 'borderless
         modus-themes-completions 'opinionated
         ;; modus-themes-intense-hl-line t
         modus-themes-paren-match 'subtle-bold
         modus-themes-headings '((1 . (background overline))
                                 (2 . (rainbow))
                                 (t . (no-bold))))
  (modus-themes-load-themes)
  :config
  ;; Load theme based on macOS dark mode status.
  (+cdom/load-os-theme))

(use-package! pragmatapro-lig
  :init
  (require 'pragmatapro-lig)
  (pragmatapro-lig-global-mode)
  :commands (pragmatapro-lig-global-mode pragmatapro-lig-mode))

(defvar +cdom/org-agenda-directory "~/org/gtd/")
(defvar +cdom/org-notes-directory "~/org/notes/")
(defvar +cdom/org-mind-directory "~/org/mind/")

(setq! org-directory "~/org"
       +org-capture-todo-file (concat +cdom/org-agenda-directory "inbox.org")
       org-roam-directory +cdom/org-mind-directory
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
(use-package! scroll-on-jump
  :after (evil)
  :config
  (setq! scroll-on-jump-duration 0.2
         scroll-on-jump-smooth t
         scroll-on-jump-use-curve nil)
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

;; https://tecosaur.github.io/emacs-config/config.html#company
(after! company
  (setq! company-idle-delay nil)
  ;; Make aborting less annoying.
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

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
         org-startup-folded t
         org-startup-with-inline-images t
         org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
         org-cycle-separator-lines -1
         org-use-property-inheritance t              ; it's convenient to have properties inherited
         org-log-done 'time                          ; log the time an item was completed
         org-log-refile 'time
         org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
         ;; org-export-in-background t                  ; run export processes in external emacs process
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

(use-package! projectile
  :config
  (appendq! projectile-globally-ignored-directories '("client-mu-plugins/vendor")))

(use-package! treemacs
  :config
  (setq! treemacs-persist-file (concat doom-private-dir "treemacs.org")))

(use-package! lsp
  :config
  (setq! lsp-phpactor-path (concat (getenv "COMPOSER_HOME") "/vendor/bin/phpactor")
         lsp-vetur-use-workspace-dependencies t)
  ;; Register rnix-lsp as a client
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

;; `lsp-mode' integration with Flycheck `sh-shellcheck' checker
;; https://old.reddit.com/r/emacs/comments/hqxm5v/weekly_tipstricketc_thread/fy4pvr8/?context=3
(defun +cdom--lsp-flycheck-enable-shellcheck ()
  "Enable Shellcheck for shell buffers under LSP."
  (when (derived-mode-p 'sh-mode)
    (flycheck-add-next-checker 'lsp 'sh-shellcheck)))
(add-hook 'lsp-after-open-hook #'+cdom--lsp-flycheck-enable-shellcheck)

;; Add multi-root workspace folders on demand.
;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; tree-sitter
;; via https://github.com/hlissner/doom-emacs-private/blob/master/modules/ui/tree-sitter/config.el
(use-package! tree-sitter
  ;; :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
    "Don't break with errors when current major mode lacks tree-sitter support."
    :around #'tree-sitter-mode
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                            (error-message-string e))
            (signal (car e) (cadr e)))))))

(use-package! literate-calc-mode
  :defer-incrementally t)

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
            :dot           "•"
            :todo          "	")

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
         "Eat protein!"))

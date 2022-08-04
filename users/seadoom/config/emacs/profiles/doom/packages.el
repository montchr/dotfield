;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))


;;; === ide ====================================================================

(unpin! lsp-mode)


;;; === completions ============================================================

;; https://github.com/elken/doom/blob/fd381b6837a34bb7b9bc072909bb697c0ac11f70/config.org#disabledunpin
(disable-packages! evil-escape)
(when (featurep! :completion corfu)
  (unpin! evil-collection))

(package! embark-vc)


;;; === ui =====================================================================

(package! fontaine)
;;(package! ligature)
(package! modus-themes)
(package! svg-tag-mode)

;; modeline
;; (package! moody)      ; a mood for any mode :: https://github.com/tarsius/moody
;; (package! minions)    ;                     :: https://github.com/tarsius/minions


;;; === org-mode ===============================================================

(package! doct)                      ; _d_eclarative _o_rg _c_apture _t_emplates
(package! vulpea)

;;; importers/exporters

;; FIXME: may have had errors at some point?
;; (package! org-protocol-capture-html) ; Capture webpage via org-protocol
(package! org-web-tools)             ; Webpage to org-mode content
(package! ox-gfm)                    ; Export to GFM syntax
(package! ox-jira                    ; Export to Jira syntax
  :recipe (:branch "trunk"))


;;; === languages ==============================================================

(package! apheleia)
(package! bats-mode)                 ; mode for editing and running BATS tests
(package! hledger-mode)
(package! literate-calc-mode)        ; like soulver, but in emacs
(package! neon-mode
  :recipe (:host github :repo "montchr/neon-mode"))
(package! vimrc-mode)                ; vimrc syntax


;;; === apps / tools ===========================================================

(package! devdocs-browser)           ; Browse devdocs.io documents using EWW

;; TODO: check it out
;; (package! org-board)              ; Pinboard-ish for org-mode

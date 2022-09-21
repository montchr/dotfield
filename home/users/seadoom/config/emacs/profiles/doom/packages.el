;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


;;; === ide ====================================================================

(unpin! lsp-mode)
(unpin! format-all)


;;; === completions ============================================================

;; https://github.com/elken/doom/blob/fd381b6837a34bb7b9bc072909bb697c0ac11f70/config.org#disabledunpin
(disable-packages! evil-escape)
(when (modulep! :completion corfu)
  (unpin! evil-collection)
  (package! embark-vc)
  (package! mini-frame))


;;; === ui =====================================================================

(package! fontaine)
(package! ligature)
(package! modus-themes)
(package! svg-tag-mode)


;;; === org-mode ===============================================================

(package! doct)                      ; _d_eclarative _o_rg _c_apture _t_emplates
(package! vulpea)

;;; === importers/exporters ====================================================

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

;; (package! devdocs-browser)           ; Browse devdocs.io documents using EWW
(package! spdx)

;; TODO: check it out
;; (package! org-board)              ; Pinboard-ish for org-mode

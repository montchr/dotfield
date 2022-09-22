;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-


(disable-packages! evil-escape)

(unpin! forge)
(unpin! lsp-mode)
(unpin! format-all)

(when (modulep! :completion corfu)
  (unpin! evil-collection))
(when (modulep! :tools lsp +eglot)
  (unpin! eglot))


(package! embark-vc)


;;; === completions ============================================================

(when (modulep! :completion company)
  (package! company-prescient))

(package! cape-yasnippet
  :recipe (:host github :repo "elken/cape-yasnippet"))


;;; === ui =====================================================================

(package! fontaine)
(package! ligature)
(package! modus-themes)
(package! svg-tag-mode)


;;; === org-mode ===============================================================

(package! doct)                      ; _d_eclarative _o_rg _c_apture _t_emplates
(package! vulpea)

;;; === importers/exporters ====================================================

(package! ox-gfm)                    ; Export to GFM syntax
(package! ox-jira                    ; Export to Jira syntax
  :recipe (:branch "trunk"))


;;; === languages ==============================================================

(package! apheleia)
(package! bats-mode)                 ; mode for editing and running BATS tests
(package! hledger-mode)
(package! literate-calc-mode)        ; like soulver, but in emacs
(package! vimrc-mode)                ; vimrc syntax


;;; === apps / tools ===========================================================

(package! devdocs-browser)           ; Browse devdocs.io documents using EWW
(package! spdx)

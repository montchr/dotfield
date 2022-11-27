;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-


(disable-packages!
 evil-escape
 solaire-mode)

(unpin! forge)
(unpin! lsp-mode)
(unpin! format-all)
(unpin! vterm)

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

(package! apache-mode)               ; a web server almost as old as emacs itself
(package! apheleia)
(package! bats-mode)                 ; mode for editing and running BATS tests
(package! hledger-mode)
(package! just-mode)                 ; justfile language support
(package! literate-calc-mode)        ; like soulver, but in emacs
(package! lsp-tailwindcss
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! robots-txt-mode)           ; 87 lines of pure metal
(package! vimrc-mode)                ; vimrc syntax


;;; === apps / tools ===========================================================

(package! devdocs-browser)           ; Browse devdocs.io documents using EWW
(package! spdx)

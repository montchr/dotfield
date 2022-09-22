;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; Disable frame decorations (titlebar + scrollbar).
(setq default-frame-alist '((undecorated . t)))
(scroll-bar-mode -1)

;; Respect visual line mode
(setq! evil-respect-visual-line-mode t)

(doom!
 :input
 ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
 ;;chinese
 ;;japanese
 ;;layout            ; auie,ctsrnm is the superior home row

 :completion
 (corfu +icons +orderless)
 (vertico +icons)
 ;;(company)

 :ui
 doom-quit                    ; DOOM quit-message prompts when you quit Emacs
 hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 hydra
 (modeline +light)
 nav-flash                    ; blink cursor line after big motions
 ophints                      ; highlight the region an operation acts on
 (popup +defaults)
 (vc-gutter)
 vi-tilde-fringe              ; fringe tildes to mark beyond EOB
 workspaces
 ;; FIXME: disabled due to visual-fill-column repo 502 error
 ;;zen                          ; distraction-free coding or writing
 ;; deft                      ; notational velocity for Emacs
 ;; doom                      ; what makes DOOM look the way it does
 ;; ligatures                 ; ligatures and symbols to make your code pretty again
 ;; (treemacs +lsp)           ; a project drawer, like neotree but cooler
 ;; window-select             ; visually switch windows

 :editor
 (evil +everywhere)
 file-templates
 fold
 rotate-text
 snippets
 word-wrap
 ;;(format)         ; automated prettiness

 :emacs
 (dired +ranger +icons)
 electric
 (ibuffer +icons)
 (undo +tree)
 vc

 :term
 eshell  
 vterm
 ;;shell   ; simple shell REPL for Emacs
 ;;term    ; basic terminal emulator for Emacs

 :checkers
 syntax
 ;; (spell +aspell)
 ;; grammar

 :tools
 (debugger +lsp)
 direnv
 (docker +lsp)
 editorconfig
 (eval +overlay)
 (lookup
  +dictionary
  +docsets
  +offline)
 (lsp +eglot)
 (magit +forge)
 (pass +auth)
 pdf
 terraform
 tmux
 tree-sitter
 upload
 ;;ansible

 :os
 (:if IS-MAC macos)  ; improve compatibility with macOS
 (tty +osc)          ; improve the terminal Emacs experience

 :lang
 data
 dhall
 (emacs-lisp +tree-sitter)
 (go +lsp)
 (haskell)
 (json +lsp +tree-sitter)
 (javascript +lsp +tree-sitter)
 ledger
 (lua +lsp)
 (nix +lsp +tree-sitter)
 (markdown +grip)
 (org
  +dragndrop
  +pandoc
  +roam2)
 (php +lsp +tree-sitter)
 (python
  +pyright
  +poetry
  +lsp +tree-sitter)
 rest
 (ruby +lsp +tree-sitter)
 (sh
   +fish
   +lsp +tree-sitter)
 (web +lsp +tree-sitter)
 (yaml +lsp)
 ;;agda              ; types of types of types of types...
 ;;beancount         ; mind the GAAP
 ;;common-lisp       ; if you've seen one lisp, you've seen them all
 ;;elm               ; care for a cup of TEA?
 ;;(graphql +lsp)    ; Give queries a REST
 ;;idris             ; a language you can depend on
 ;;nim               ; python + lisp at the speed of c
 ;;ocaml             ; an objective camel
 ;;plantuml          ; diagrams for confusing people more
 ;;purescript        ; javascript, but functional
 ;;rst               ; ReST in peace
 ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
 ;;scheme            ; a fully conniving family of lisps
 ;;swift             ; who asked for emoji variables?
 ;;zig               ; C, but simpler

 :email
 (mu4e +org)
 ;;notmuch
 ;;(wanderlust +gmail)

 :app
 ;;calendar
 ;;emms
 everywhere
 (rss +org)

 :config
 ;;literate
 (default +bindings +smartparens))

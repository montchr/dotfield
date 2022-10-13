;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; Disable frame decorations (titlebar + scrollbar).
(setq default-frame-alist '((undecorated . t)))
(scroll-bar-mode -1)

;; Respect visual line mode
(setq! evil-respect-visual-line-mode t)

;; Performance improvement for lsp-mode.
(setq! lsp-use-plists t)

;; Restore ahead-of-time native compilation behavior.
;; https://github.com/doomemacs/doomemacs/issues/6811#issuecomment-1250350426
(setq native-comp-deferred-compilation nil)
(after! (doom-packages straight)
  (setq straight--native-comp-available t))

(doom!
 :input
 ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
 ;;chinese
 ;;japanese
 ;;layout            ; auie,ctsrnm is the superior home row

 :completion
 (corfu
  ;; +icons ; FIXME: icon height does not scale with font size -- also causes lag
  +orderless)
 (vertico +icons)
 ;;(company)

 :ui
 deft                         ; notational velocity for Emacs
 doom-quit                    ; DOOM quit-message prompts when you quit Emacs
 hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 hydra
 (modeline +light)
 ;; nav-flash                    ; blink cursor line after big motions
 ophints                      ; highlight the region an operation acts on
 (popup +all
        +defaults)
 (vc-gutter)
 vi-tilde-fringe              ; fringe tildes to mark beyond EOB
 workspaces
 ;; FIXME: disabled due to visual-fill-column repo 502 error
 ;;zen                          ; distraction-free coding or writing
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
 (:if (executable-find "aspell")
   spell +aspell)
 ;; grammar

 :tools
 (debugger +lsp)
 direnv
 (docker +lsp)
 editorconfig
 (eval +overlay)
 (lookup +docsets)
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
 (graphql +lsp)
 data
 dhall
 (emacs-lisp +tree-sitter)
 (json +lsp
       +tree-sitter)
 (javascript +lsp
             +tree-sitter)
 ledger
 (lua +lsp)
 (nix +lsp
      +tree-sitter)
 (markdown +grip)
 (org
  +dragndrop
  +pandoc
  +roam2)
 (php +lsp
      +tree-sitter)
 (python
  +pyright
  +poetry
  +lsp
  +tree-sitter)
 rest
 (ruby +lsp
       +tree-sitter)
 (rust +lsp)
 (sh
  +fish
  +lsp
  +tree-sitter)
 (web +lsp
      +tree-sitter)
 (yaml +lsp)
 ;;agda              ; types of types of types of types...
 ;;beancount         ; mind the GAAP
 ;;common-lisp       ; if you've seen one lisp, you've seen them all
 ;;elm               ; care for a cup of TEA?
 ;;(go +lsp)
 ;;(haskell)
 ;;idris             ; a language you can depend on
 ;;nim               ; python + lisp at the speed of c
 ;;ocaml             ; an objective camel
 ;;plantuml          ; diagrams for confusing people more
 ;;purescript        ; javascript, but functional
 ;;rst               ; ReST in peace
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

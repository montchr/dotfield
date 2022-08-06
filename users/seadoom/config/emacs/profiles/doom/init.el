;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; GNU Emacs 29+ only: Disable titlebar on frames.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/433#issuecomment-1025547880
(when EMACS29+ (add-to-list 'default-frame-alist '(undecorated . t)))

;; Respect visual line mode
(setq! evil-respect-visual-line-mode t)

(doom!
  :input
  ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
  ;;chinese
  ;;japanese
  ;;layout            ; auie,ctsrnm is the superior home row

  :completion
  (corfu
    +icons
    +minibuffer
    +orderless)
  ;;(company +childframe)      ; the ultimate code completion backend
  ;;helm                       ; the *other* search engine for love and life
  ;;ido                        ; the other *other* search engine...
  ;;(ivy +prescient +icons)    ; a search engine for love and life
  (vertico +icons)             ; the search engine of the future

  :ui
  ;; deft                      ; notational velocity for Emacs
  ;; doom                      ; what makes DOOM look the way it does
  ;; doom-dashboard            ; a nifty splash screen for Emacs
  doom-quit                    ; DOOM quit-message prompts when you quit Emacs
  ;; (emoji +unicode)          ; 🙂
  ;; fill-column               ; a `fill-column' indicator
  hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
  hydra
  ;; indent-guides             ; highlighted indent columns
  ;; minimap                   ; show a map of the code on the side
  modeline                     ; snazzy, Atom-inspired modeline, plus API
  nav-flash                    ; blink cursor line after big motions
  ;; neotree                   ; a project drawer, like NERDTree for vim
  ophints                      ; highlight the region an operation acts on
  (popup +defaults)            ; tame sudden yet inevitable temporary windows
  ;; tabs                      ; an tab bar for Emacs
  ;; (treemacs +lsp)           ; a project drawer, like neotree but cooler
  ;; unicode                   ; extended unicode support for various languages
  vc-gutter                    ; vcs diff in the fringe
  ;; vi-tilde-fringe           ; fringe tildes to mark beyond EOB
  ;; window-select             ; visually switch windows
  workspaces                   ; tab emulation, persistence & separate workspaces
  zen                          ; distraction-free coding or writing

  :editor
  (evil +everywhere) ; come to the dark side, we have cookies
  file-templates     ; auto-snippets for empty files
  fold               ; (nigh) universal code folding
  ;;(format)         ; automated prettiness
  ;;god              ; run Emacs commands without modifier keys
  ;;lispy            ; vim for lisp, for people who don't like vim
  ;;multiple-cursors ; editing in many places at once
  ;;objed            ; text object editing for the innocent
  ;;(parinfer +rust) ; turn lisp into python, sort of
  rotate-text        ; cycle region at point between text candidates
  snippets           ; my elves. They type so I don't have to
  word-wrap          ; soft wrapping with language-aware indent

  :emacs
  (dired +ranger +icons) ; making dired pretty [functional]
  electric               ; smarter, keyword-based electric-indent
  (ibuffer +icons)       ; interactive buffer management
  (undo +tree)           ; persistent, smarter undo for your inevitable mistakes
  vc                     ; version-control and Emacs, sitting in a tree

  :term
  eshell    ; the elisp shell that works everywhere
  ;;shell   ; simple shell REPL for Emacs
  ;;term    ; basic terminal emulator for Emacs
  vterm     ; the best terminal emulation in Emacs

  :checkers
  (syntax +childframe)   ; tasing you for every semicolon you forget
  ;; (spell +aspell)     ; tasing you for misspelling mispelling
  ;; grammar  ; tasing grammar mistake every you make

  :tools
  (debugger +lsp)    ; FIXME stepping through code, to help you add bugs
  direnv
  (docker)
  editorconfig       ; let someone else argue about tabs vs spaces
  (eval +overlay)    ; run code, run (also, repls)
  gist               ; interacting with github gists
  (lookup            ; navigate your code and its documentation
    +dictionary
    +offline)
  (lsp)              ; M-x vscode
  (magit +forge)     ; a git porcelain for Emacs
  (pass +auth)       ; password manager for nerds
  pdf                ; pdf enhancements
  terraform          ; infrastructure as code
  tmux               ; an API for interacting with tmux
  tree-sitter        ; syntax and parsing, sitting in a tree...
  upload             ; map local to remote projects via ssh/ftp
  ;;ansible
  ;;biblio           ; Writes a PhD for you (citation needed)
  ;;ein              ; tame Jupyter notebooks with emacs
  ;;make             ; run make tasks from Emacs
  ;;prodigy          ; FIXME managing external services & code builders
  ;;rgb              ; creating color strings
  ;;taskrunner       ; taskrunner for all your projects

  :os
  (:if IS-MAC macos)  ; improve compatibility with macOS
  (tty +osc)          ; improve the terminal Emacs experience

  :lang
  data                 ; config/data formats
  (emacs-lisp          ; drown in parentheses
    +tree-sitter)
  (json                ; At least it ain't XML
    +lsp
    +tree-sitter)
  (javascript          ; all(hope(abandon(ye(who(enter(here))))))
    +lsp
    +tree-sitter)
  ledger               ; an accounting system in Emacs
  (lua +lsp)           ; one-based indices? one-based indices
  (markdown +grip)     ; writing docs for people to ignore
  nix                  ; I hereby declare "nix geht mehr!"
  (org                 ; organize your plain life in plain text
    +dragndrop
    +pandoc
    +roam2)
  (php +lsp)           ; perl's insecure younger brother
  (python              ; beautiful is better than ugly
    +lsp
    +poetry
    +tree-sitter)
  rest                 ; Emacs as a REST client
  (ruby                ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
    +lsp
    +tree-sitter)
  (scheme +guile)      ; a fully conniving family of lisps
  (sh +fish +lsp)      ; she sells {ba,z,fi}sh shells on the C xor
  (web +lsp)           ; the tubes
  (yaml +lsp)          ; JSON, but readable
  ;;agda               ; types of types of types of types...
  ;;cc                 ; C/C++/Obj-C madness
  ;;beancount          ; mind the GAAP
  ;;cc                 ; C > C++ == 1
  ;;clojure            ; java with a lisp
  ;;common-lisp        ; if you've seen one lisp, you've seen them all
  ;;coq                ; proofs-as-programs
  ;;crystal            ; ruby at the speed of c
  ;;csharp             ; unity, .NET, and mono shenanigans
  ;;(dart +flutter)    ; paint ui and not much else
  ;;dhall
  ;;elixir             ; erlang done right
  ;;elm                ; care for a cup of TEA?
  ;;erlang             ; an elegant language for a more civilized age
  ;;ess                ; emacs speaks statistics
  ;;factor
  ;;faust              ; dsp, but you get to keep your soul
  ;;fortran            ; in FORTRAN, GOD is REAL (unless declared INTEGER)
  ;;fsharp             ; ML stands for Microsoft's Language
  ;;fstar              ; (dependent) types and (monadic) effects and Z3
  ;;gdscript           ; the language you waited for
  ;;(go +lsp)          ; the hipster dialect
  ;;(haskell +lsp)     ; a language that's lazier than I am
  ;;hy                 ; readability of scheme w/ speed of python
  ;;idris              ;
  ;;(java +meghanada)  ; the poster child for carpal tunnel syndrome
  ;;julia              ; a better, faster MATLAB
  ;;kotlin             ; a better, slicker Java(Script)
  ;;(latex +fold)      ; writing papers in Emacs has never been so fun
  ;;lean
  ;;nim                ; python + lisp at the speed of c
  ;;ocaml              ; an objective camel
  ;;perl                ; write code no one else can comprehend
  ;;plantuml            ; diagrams for confusing people more
  ;;purescript          ; javascript, but functional
  ;;qt                  ; the 'cutest' gui framework ever
  ;;racket              ; a DSL for DSLs
  ;;raku                ; the artist formerly known as perl6
  ;;rst                 ; ReST in peace
  ;;rust                ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
  ;;scala               ; java, but good
  ;;sml
  ;;solidity            ; do you need a blockchain? No.
  ;;swift               ; who asked for emoji variables?
  ;;terra               ; Earth and Moon in alignment for performance.
  ;;zig                 ; C, but simpler

  :email
  (mu4e +org)
  ;;notmuch
  ;;(wanderlust +gmail)

  :app
  ;;calendar
  ;;emms
  everywhere     ; *leave* Emacs!? You must be joking
  ;;irc          ; how neckbeards socialize
  (rss +org)     ; emacs as an RSS reader
  ;;twitter      ; twitter client https://twitter.com/vnought

  :config
  ;;literate
  (default +smartparens +bindings))

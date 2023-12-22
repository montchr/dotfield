# NOTE: in some cases, the nixpkgs emacs package might still be preferable when
#       it has some exotic setup/integration required
#       <https://github.com/NixOS/nixpkgs/tree/nixos-unstable/pkgs/applications/editors/emacs/elisp-packages/manual-packages>
# FIXME: should use versions from emacs-overlay but without overlay
epkgs:
with epkgs; [
  # FIXME: "undefined variable"
  # org-modern-indent

  ace-window
  all-the-icons
  annalist
  anzu
  apache-mode
  async
  avy
  blackout
  browse-at-remote
  burly
  cape
  cfrs
  circadian
  compat
  consult
  consult-lsp
  consult-gh
  consult-notes
  corfu
  csv-mode
  dash
  denote
  denote-menu
  devdocs
  diff-hl
  diredfl
  dirvish
  doct
  doom-modeline
  drag-stuff
  dumb-jump
  editorconfig #req: editorconfig-c
  elisp-demos
  elisp-refs
  embark
  embark-consult
  embrace
  envrc # req: direnv
  epl
  evil
  evil-anzu
  evil-args
  evil-collection
  evil-embrace
  evil-escape
  evil-exchange
  evil-goggles
  evil-indent-plus
  evil-lion
  evil-markdown
  evil-matchit
  evil-nerd-commenter
  evil-numbers
  evil-quickscope
  evil-surround
  evil-traces
  evil-visualstar
  exato
  exec-path-from-shell
  expand-region
  f
  flycheck
  fontaine
  fullframe
  git-commit
  git-timemachine
  goto-chg
  helpful
  hierarchy
  hl-todo
  hledger-mode # req: hledger
  ht
  htmlize
  hydra
  iedit
  inheritenv
  ivy
  json-navigator
  just-mode
  kind-icon
  ligature
  link-hint
  lispy
  lispyville
  llama
  loop
  lsp-mode
  lsp-ui
  lua-mode
  lv
  magit # req: git
  magit-section
  marginalia
  markdown-mode
  modus-themes
  nerd-icons # FIXME: only for graphical
  nix-mode
  no-littering
  nushell-mode
  olivetti
  orderless
  org
  org-cliplink
  org-modern
  org-rich-yank
  page-break-lines
  pandoc-mode # req: pandoc
  pfuture
  pkg-info
  popon
  popper
  popup
  posframe
  pretty-hydra
  projectile
  rainbow-delimiters
  rainbow-mode
  reformatter
  robots-txt-mode
  s
  scratch
  shrink-path
  smartparens
  snap-indent
  spacious-padding
  spinner
  suggest
  svg-lib
  swiper
  tempel
  tempel-collection
  treemacs
  treemacs-magit
  treemacs-projectile
  undo-fu # rec: zstd
  undo-fu-session
  use-package
  use-package-hydra
  vertico
  vimrc-mode
  vundo
  web-mode
  wgrep # req: coreutils, ripgrep
  which-key
  with-editor
  yaml-mode
  zoutline

  #: init-lang-php
  php-mode # req: php

  #: init-lang-js
  rjsx-mode
  typescript-mode
  js2-refactor

  #: init-treesit
  treesit-auto
  treesit-grammars.with-all-grammars
]

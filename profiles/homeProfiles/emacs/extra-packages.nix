# FIXME: split apart
{pkgs, ...}: {
  home.packages = with pkgs; [
    gnutls
    (ripgrep.override {withPCRE2 = true;})

    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    #: org
    graphviz
    sqlite

    editorconfig-core-c

    ##: === writing ===

    (aspellWithDicts (ds:
      with ds; [
        en
        en-computers
        en-science
      ]))
    languagetool

    #: lookup +docsets
    wordnet

    ##: === lang/lsp ===

    # typescript is a required peer dependency for many language servers.
    nodePackages.typescript

    #: docker
    nodePackages.dockerfile-language-server-nodejs

    #: terraform
    terraform
    terraform-ls

    #: css
    nodePackages.vscode-css-languageserver-bin

    #: js
    nodePackages.eslint
    nodePackages.typescript-language-server

    #: json
    nodePackages.vscode-json-languageserver

    #: ledger
    # FIXME: marked as broken upstream
    # ledger

    #: markdown
    nodePackages.unified-language-server

    #: php
    nodePackages.intelephense

    #: ruby
    rubyPackages.solargraph

    #: rust +lsp
    rust-analyzer

    #: rst
    rst2pdf

    #: sh
    nodePackages.bash-language-server

    #: tailwindcss
    nodePackages.tailwindcss

    #: toml
    taplo-lsp

    #: web-mode
    nodePackages.js-beautify
    nodePackages.stylelint
    nodePackages.vscode-html-languageserver-bin
    #: yaml
    nodePackages.yaml-language-server
    #: vimrc
    nodePackages.vim-language-server
  ];
}

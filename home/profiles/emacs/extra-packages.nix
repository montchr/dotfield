# FIXME: split apart
# FIXME: add these tools as necessary in other profiles
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gnutls
    (ripgrep.override { withPCRE2 = true; })
    fd

    imagemagick # for image-dired
    zstd # for compression in supported contexts

    editorconfig-core-c

    ##: === writing ===

    (aspellWithDicts (
      ds: with ds; [
        en
        en-computers
        en-science
      ]
    ))
    languagetool

    ##: === lang/lsp ===

    # typescript is a required peer dependency for many language servers.
    nodePackages.typescript

    #: docker
    nodePackages.dockerfile-language-server-nodejs

    #: css
    nodePackages.vscode-css-languageserver-bin

    #: js
    nodePackages.eslint
    nodePackages.typescript-language-server

    #: json
    nodePackages.vscode-json-languageserver

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

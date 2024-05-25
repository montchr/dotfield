{ pkgs, ... }:
{
  home.packages =
    [
      (pkgs.ripgrep.override { withPCRE2 = true; })
      pkgs.imagemagick # for image-dired
      pkgs.zstd # for compression in supported contexts

      pkgs.editorconfig-core-c
      pkgs.taplo-lsp # toml language server
    ]
    ++ (with pkgs.nodePackages; [
      bash-language-server
      dockerfile-language-server-nodejs
      typescript-language-server
      vscode-css-languageserver-bin
      vscode-html-languageserver-bin
      vscode-json-languageserver
      yaml-language-server
    ]);
}

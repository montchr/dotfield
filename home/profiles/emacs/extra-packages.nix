{ pkgs, ... }:
{
  home.packages =
    [
      (pkgs.ripgrep.override { withPCRE2 = true; })
      pkgs.imagemagick # for image-dired
      pkgs.zstd # for compression in supported contexts

      pkgs.bash-language-server
      pkgs.editorconfig-core-c
      pkgs.taplo-lsp # toml language server
    ]
    ++ (with pkgs.nodePackages; [
      dockerfile-language-server-nodejs
      typescript-language-server
      vscode-langservers-extracted
      yaml-language-server
    ]);
}

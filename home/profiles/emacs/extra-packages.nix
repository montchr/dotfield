# FIXME: split apart
# FIXME: add these tools as necessary in other profiles
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })

    imagemagick # for image-dired
    zstd # for compression in supported contexts

    editorconfig-core-c

    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.typescript-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    nodePackages.vscode-json-languageserver
    nodePackages.yaml-language-server
    taplo-lsp # toml

    # nodePackages.intelephense # php (unfree)
  ];
}

{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    (python3.withPackages (ps:
      with ps; [
        black
        grip
        pip
        pyright # lsp server
      ]))
  ];
}

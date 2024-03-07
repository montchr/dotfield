{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (python3.withPackages (
      ps: with ps; [
        black
        pip
        pyright # lsp server
      ]
    ))
  ];
}

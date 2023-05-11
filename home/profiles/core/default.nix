{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./__bat.nix
    ./__direnv.nix
    ./__fzf.nix
    ./__help.nix
    ./__nixpkgs.nix
    ./__nnn.nix
    ./__shell.nix
    ./__utils.nix
    ./__xdg-dirs.nix
  ];

  ##: home-manager setup
  programs.home-manager.enable = true;
  manual.json.enable = true;
  news.display = "show";
}

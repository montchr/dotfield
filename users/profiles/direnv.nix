{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    # Prevent early loading of direnv integration for a faster shell startup.
    enableZshIntegration = false;
  };
}

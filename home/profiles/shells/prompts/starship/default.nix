{ lib, ... }:
{
  # imports = [ ./extra-symbols.nix ];
  programs.starship = {
    enable = true;
  };
}

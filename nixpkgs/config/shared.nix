# This file contains configuration that is shared across all hosts.
{ pkgs, lib, options, ... }: {
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hardselius.cachix.org"
      "https://hydra.iohk.io"
      "https://iohk.cachix.org"
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      # "hardselius.cachix.org-1:PoN90aQw2eVMwfAy0MS6V9T2exWlgtHOUBBSnthXAl4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    ];
  };

  programs = {
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  # fonts = (lib.mkMerge [
  #   # NOTE: Remove this condition when `nix-darwin` aligns with NixOS
  #   (if (builtins.hasAttr "fontDir" options.fonts) then {
  #     fontDir.enable = true;
  #   } else {
  #     enableFontDir = true;
  #   })
  #   { fonts = with pkgs; [ hack-font ]; }
  # ]);
}

{
  aspects.graphical.nixos =
    { pkgs, ... }:
    {
      stylix.enable = true;
      stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    };
}

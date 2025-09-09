{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.dconf2nix
        pkgs.dconf-editor
      ];
    };
}

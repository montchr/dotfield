{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.eog # "Eye of GNOME" image viewer
        pkgs.kdePackages.koko
        pkgs.pix
      ];
    };
}

{
  aspects.core.home = {
    programs.nnn.enable = true;
  };

  aspects.graphical.home =
    { pkgs, ... }:
    {
      programs.nnn.extraPackages = [
        pkgs.fontpreview
        pkgs.poppler
        pkgs.viu
        pkgs.w3m # text-mode web browser
      ];
    };
}

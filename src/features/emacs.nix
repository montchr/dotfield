{ lib, ... }:
{
  dotfield.aspects.baseline.home =
    { pkgs, ... }:
    {
      programs.emacs.enable = true;
      programs.emacs.package = lib.mkDefault pkgs.emacs-unstable;
    };

  dotfield.aspects.graphical.home =
    { pkgs, ... }:
    {
      programs.emacs = {
        package =
          if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
      };
    };
}

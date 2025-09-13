{ inputs, ... }:
{
  aspects.development.home =
    { lib, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
    in
    {
      imports = [
        inputs.ceamx.modules.homeManager.ceamx
      ];

      programs.emacs = {
        enable = true;
        package = if isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
      };

      stylix.targets.emacs.enable = false;
    };
}

{
  withSystem,
  lib,
  inputs,
  ...
}:
{
  dotfield.users.cdom.features.development.home =
    { pkgs, ... }:
    withSystem pkgs.stdenv.hostPlatform.system (
      { packages, inputs', ... }:
      let
        inherit (pkgs.stdenv.hostPlatform) isDarwin;
        sessionVariables = {
          EDITOR = lib.getExe inputs'.ceamx.packages.editor;
        };
      in
      {
        imports = [ inputs.ceamx.modules.home.ceamx ];

        programs.emacs = {
          enable = true;
          package = if isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
          ceamx.enable = true;
        };

        home = { inherit sessionVariables; };
        programs.bash = { inherit sessionVariables; };

        stylix.targets.emacs.enable = false;
      }
    );
}

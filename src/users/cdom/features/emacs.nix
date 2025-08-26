{
  moduleWithSystem,
  lib,
  inputs,
  ...
}:
{
  dotfield.users.cdom.aspects.development.home = moduleWithSystem (
    perSystem@{ inputs', ... }:
    home@{ pkgs, ... }:

    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
      sessionVariables = {
        EDITOR = lib.getExe inputs'.ceamx.packages.editor;
      };
    in
    {
      imports = [ inputs.ceamx.modules.homeManager.ceamx ];

      programs.emacs = {
        enable = true;
        ceamx.enable = true;
      };

      home = { inherit sessionVariables; };
      programs.bash = { inherit sessionVariables; };

      stylix.targets.emacs.enable = false;
    }
  );
}

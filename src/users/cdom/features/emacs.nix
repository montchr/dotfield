{ lib, moduleWithSystem, ... }:
{
  users.cdom.aspects.development.home = moduleWithSystem (
    perSystem@{ inputs' }:
    home@{ pkgs, ... }:
    let
      sessionVariables = {
        EDITOR = lib.getExe perSystem.inputs'.ceamx.packages.editor;
      };
    in
    {
      programs.emacs.ceamx.enable = true;

      home = {
        inherit sessionVariables;
      };
      programs.bash = {
        inherit sessionVariables;
      };
    }
  );
}

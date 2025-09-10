{ inputs, moduleWithSystem, ... }:
{
  aspects.development.home = moduleWithSystem (
    perSystem@{ inputs' }:
    home@{
      lib,
      pkgs,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;

      sessionVariables = {
        EDITOR = lib.getExe perSystem.inputs'.ceamx.packages.editor;
      };
    in
    {
      imports = [ inputs.ceamx.modules.homeManager.ceamx ];

      programs.emacs = {
        enable = true;
        package = if isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
        ceamx.enable = true;
      };

      home = {
        inherit sessionVariables;
      };
      programs.bash = {
        inherit sessionVariables;
      };

      stylix.targets.emacs.enable = false;
    }
  );
}

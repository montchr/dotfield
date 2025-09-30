{ lib, moduleWithSystem, ... }:
{
  aspects.development.home = moduleWithSystem (
    perSystem@{ config }:
    { pkgs, ... }:
    {
      home.packages = [ perSystem.config.packages.difftastic-16k ];

      programs.git.extraConfig = {
        diff.external = lib.mkDefault "difft";
        diff.tool = lib.mkDefault "difftastic";
        difftool.difftastic.cmd = ''difft "$MERGED" "$LOCAL" "abcdef1" "100644" "$REMOTE" "abcdef2" "100644"'';
      };

      programs.jujutsu.settings = {
        ui = {
          diff-formatter = [
            "difft"
            "--color=always"
            "$left"
            "$right"
          ];
        };
      };
    }
  );
}

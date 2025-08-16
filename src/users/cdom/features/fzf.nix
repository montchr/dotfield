{
  withSystem,
  lib,
  ...
}:
let
  packageCommand =
    pkg: args:
    lib.concatStringsSep " " [
      (lib.getExe pkg)
      ((lib.cli.toGNUCommandLine { } args) |> builtins.toString)
    ];
in

{
  dotfield.baseline.home =
    nixos@{ pkgs, ... }:
    (withSystem pkgs.stdenv.hostPlatform.system (
      perSystem@{ config }:
      let
        fd = packageCommand pkgs.fd;
        findFiles = args: fd (args // { type = "f"; });
        findDirs = args: fd (args // { type = "d"; });
        dirPreviewCommand = lib.getExe pkgs.eza + " --tree {} | head -n 200";
      in
      {
        home.packages = [ config.packages.igr ];

        programs.fzf = {
          enable = true;
          defaultOptions = [
            "--ansi"
            "--reverse"
            "--border"
            "--inline-info"
            "--color=16"
          ];
          fileWidgetCommand = findFiles {
            hidden = true;
            follow = true;
            exclude = [
              ".git"
              ".devenv"
              ".direnv"
              ".jj"
              "node_modules"
              "vendor"
            ];
          };
          # TODO: use `bat` -- see `igr` package source for example (doesn't include `head`-like tho)
          fileWidgetOptions = [ "--preview 'head {}'" ];
          changeDirWidgetCommand = findDirs { };
          changeDirWidgetOptions = [
            "--tiebreak=index"
            "--preview '${dirPreviewCommand}'"
          ];
          historyWidgetOptions = [
            "--sort"
            "--exact"
          ];
        };
      }
    ));

}

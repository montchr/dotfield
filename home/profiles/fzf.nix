{
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (builtins) concatStringsSep toString;

  ## TODO: outfactor these to lib?
  packageCommand =
    pkg: args:
    concatStringsSep " " [
      (lib.getExe pkg)
      (toString (lib.cli.toGNUCommandLine { } args))
    ];

  find = packageCommand pkgs.fd;
  findFiles = args: find (args // { type = "f"; });
  findDirs = args: find (args // { type = "d"; });
  # list = packageCommand pkgs.eza;

  dirPreviewCommand = lib.getExe pkgs.eza + " --tree {} | head -n 200";
in
{
  home.packages = [ flake.packages.igr ];

  programs.fzf = {
    enable = true;
    defaultOptions = [
      "--ansi"
      "--reverse"
      "--border"
      "--inline-info"
      "--color=16"
    ];

    ##: --- files ---

    fileWidgetCommand = findFiles {
      hidden = true;
      follow = true;
      exclude = [
        ".git"
        ".devenv"
        ".direnv"
        ".std"
        "node_modules"
        "vendor"
      ];
    };
    # TODO: use `bat` -- see `igr` package source for example (doesn't include `head`-like tho)
    fileWidgetOptions = [ "--preview 'head {}'" ];

    ##: --- directories ---

    changeDirWidgetCommand = findDirs { };
    changeDirWidgetOptions = [
      "--tiebreak=index"
      "--preview '${dirPreviewCommand}'"
    ];

    ##: --- history ---

    historyWidgetOptions = [
      "--sort"
      "--exact"
    ];
  };
}

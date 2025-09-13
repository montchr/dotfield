{ lib, ... }:
let
  cmd =
    pkg: args:
    lib.concatStringsSep " " [
      (lib.getExe pkg)
      (toString (lib.cli.toGNUCommandLine { } args))
    ];
in
{
  flake.lib.shell = {
    inherit cmd;
  };
}

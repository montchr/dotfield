{ pkgs, flake, ... }:
let
  l = flake.inputs.nixpkgs.lib // builtins;

  ## TODO: outfactor these to lib?
  packageCommand =
    pkg: args:
    l.concatStringsSep " " [
      (l.getExe pkg)
      (l.toString (l.cli.toGNUCommandLine { } args))
    ];
  find = packageCommand pkgs.fd;
  findFiles = args: find (args // { type = "f"; });
  findDirs = args: find (args // { type = "d"; });
  # list = packageCommand pkgs.eza;

  dirPreviewCommand = l.getExe pkgs.eza + " --tree {} | head -n 200";
in
{

}

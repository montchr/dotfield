{ flake, ... }:
let
  inherit (flake.perSystem) packages;
in
{
  imports = [
    ./git.nix
    ./ssh.nix
  ];

  home.packages = [ packages.wp-to-psr-4 ];
}

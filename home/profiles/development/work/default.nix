{ flake, ... }:
let
  inherit (flake.perSystem) packages;
in
{
  imports = [
    ../common.nix
    ../aws.nix
    ../php.nix

    ./git.nix
    ./ssh.nix
  ];

  home.packages = [ packages.wp-to-psr-4 ];
}

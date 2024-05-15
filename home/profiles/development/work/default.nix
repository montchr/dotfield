{ pkgs, ... }:
{
  imports = [
    ../common.nix
    ../aws.nix
    ../php.nix

    ./ssh.nix
  ];
}

{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.mosh.enable = true;
  programs.ssh.startAgent = true;
}

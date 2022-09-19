{
  config,
  lib,
  pkgs,
  ...
}: {
  # home.packages = with pkgs; [podman];
  home.shellAliases."docker" = "podman";
  home.shellAliases."docker-compose" = "podman-compose";
}

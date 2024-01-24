{
  lib,
  config,
  pkgs,
  ...
}: let
  isAutoLoginEnabled = config.services.xserver.displayManager.autoLogin.enable;
in {
  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  services.gnome.core-developer-tools.enable = true;
  services.gnome.games.enable = true;
  services.gnome.gnome-browser-connector.enable = true;

  # Prefer webmail or other (better) mail clients.
  environment.gnome.excludePackages = [pkgs.gnome.geary];

  environment.systemPackages = with pkgs.gnomeExtensions; [
    clipboard-history # <https://github.com/SUPERCILEX/gnome-clipboard-history>
  ];

  home-manager.sharedModules = lib.singleton {
    imports = lib.singleton ../../homeProfiles/desktop/gnome/common.nix;
  };

  # Prevent GNOME session crashes when auto-login is enabled.
  # <https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229>
  # TODO: should not be "default", should be normal
  systemd.services."getty@tty1".enable = lib.mkDefault (!isAutoLoginEnabled);
  systemd.services."autovt@tty1".enable = lib.mkDefault (!isAutoLoginEnabled);

  programs.gnupg.agent.pinentryFlavor = "gnome3";
}

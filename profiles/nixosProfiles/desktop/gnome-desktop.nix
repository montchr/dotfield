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
  services.gnome.sushi.enable = true;
  # FIXME: has no effect, prob cos gpg-agent is configured in home-manager?

  # Prevent GNOME session crashes when auto-login is enabled.
  # <https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229>
  systemd.services."getty@tty1".enable = lib.mkDefault (!isAutoLoginEnabled);
  systemd.services."autovt@tty1".enable = lib.mkDefault (!isAutoLoginEnabled);

  programs.gnupg.agent.pinentryFlavor = "gnome3";
  # Required for browser integrations with GNOME services.
  services.gnome.gnome-browser-connector.enable = true;
}

{
  lib,
  config,
  pkgs,
  ...
}:
let
  isAutoLoginEnabled = config.services.displayManager.autoLogin.enable;
in
{
  imports = [ ./common.nix ];

  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  services.gnome.core-developer-tools.enable = true;
  services.gnome.games.enable = true;
  services.gnome.gnome-browser-connector.enable = true;

  # Prefer webmail or other (better) mail clients.
  environment.gnome.excludePackages = [ pkgs.gnome.geary ];

  # Add GNOME Extensions:
  environment.systemPackages = with pkgs.gnomeExtensions; [
    clipboard-history # https://extensions.gnome.org/extension/4839/clipboard-history/
    just-perfection # https://gitlab.gnome.org/jrahmatzadeh/just-perfection
  ];

  home-manager.sharedModules = lib.singleton {
    imports = [ ../../../home/profiles/graphical/sessions/gnome/common.nix ];
  };

  # Prevent GNOME session crashes when auto-login is enabled.
  # <https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229>
  systemd.services."getty@tty1".enable = (!isAutoLoginEnabled);
  systemd.services."autovt@tty1".enable = (!isAutoLoginEnabled);

  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;
}

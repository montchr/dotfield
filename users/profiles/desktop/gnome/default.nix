{
  config,
  lib,
  pkgs,
  ...
}: {
  # imports = [./dconf.settings.nix];
  config = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    home.packages = with pkgs; [
      # https://github.com/gvolpe/dconf2nix
      dconf2nix
    ];
    qt.platformTheme = "gnome";
    qt.style.package = pkgs.adwaita-qt;
    # FIXME: dark mode
    qt.style.name = "adwaita";
    services.gnome-keyring.enable = false;
    # services.pulseeffects = ...
  };
}

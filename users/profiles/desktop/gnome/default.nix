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

    # https://github.com/NixOS/nixpkgs/issues/174099
    services.gnome-keyring.enable = false;

    # TODO
    # services.pulseeffects = ...
  };
}

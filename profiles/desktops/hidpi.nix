{
  config,
  pkgs,
  lib,
  ...
}: {
  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 170;

  # Note that at the time of writing this is not currently loaded in Wayland
  # sessions.
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
        Xft.dpi: 192
        Xcursor.theme: Adwaita
        Xcursor.size: 64
    EOF
  '';

  console = {
    font = "ter-i32b";
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };
}
# === sources ===
# https://github.com/srid/nixos-config/blob/master/nixos/desktopish/hidpi.nix


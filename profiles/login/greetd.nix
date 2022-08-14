# FIXME: cannot boot into gnome!
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.sys) hasNvidia;
  inherit (config.lib.dotfield.home) hasWm;

  hasGnome = config.services.xserver.desktopManager.gnome.enable;
  hasSteam = config.programs.steam.enable;
  hasSway = config.programs.sway.enable or (hasWm "sway");
  mkSession = name: pkgs.writeShellScriptBin "dotfield-session-${name}";

  # TODO: Once the `nvidia-open` drivers are in a stable place, hopefully we can
  # remove this...
  swayFlags = lib.optionalString hasNvidia "--unsupported-gpu";

  # If some users aren't able to shutdown/reboot, refer to the following:
  # https://github.com/apognu/tuigreet#power-management
  #
  # FIXME: swaynag options here are not keyboard-focusable! without a mouse,
  # that means there's no way to select them...
  sway-kiosk = command: ''
    ${pkgs.sway}/bin/sway ${swayFlags} --config \
      ${pkgs.writeText "kiosk.config"
      ''
        output * bg #000000 solid_color
        exec "${command}; ${pkgs.sway}/bin/swaymsg exit"

        bindsym Mod4+shift+e exec ${pkgs.sway}/bin/swaynag \
          -t warning \
          -m 'What do you want to do?' \
          -b 'Poweroff' 'systemctl poweroff' \
          -b 'Reboot' 'systemctl reboot'
      ''};
  '';

  sway = mkSession "sway" ''
    ${pkgs.sway}/bin/sway ${swayFlags}
  '';
  gnome = mkSession "gnome" ''
    export XDG_SESSION_TYPE=wayland
    export MOZ_ENABLE_WAYLAND=1
    export QT_QPA_PLATFORM=wayland
    ${pkgs.gnome.gnome-session}/bin/gnome-session
  '';
  steam-bigpicture = mkSession "steam-bigpicture" ''
    ${sway-kiosk "${pkgs.steam}/bin/steam -bigpicture"}
  '';
in {
  environment.systemPackages = [
    pkgs.bashInteractive
    pkgs.fish
    (lib.mkIf hasGnome gnome)
    (lib.mkIf hasSteam steam-bigpicture)
    (lib.mkIf hasSway sway)
  ];

  environment.etc."greetd/environments".text =
    (lib.optionalString hasSway "${sway.name}\n")
    + (lib.optionalString hasGnome "${gnome.name}\n")
    + "fish\n"
    + "bash\n"
    + (lib.optionalString hasSteam "${steam-bigpicture.name}\n");

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = sway-kiosk "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l";
      };
      # initial_session = lib.mkIf config.dotfield.guardian.enable {
      #   user = config.dotfield.guardian.username;
      #   command =
      #     if hasSway
      #     then sway.name
      #     else if hasGnome
      #     then gnome.name
      #     else "${pkgs.fish}/bin/fish -l";
      # };
    };
  };
}
##: Sources
# - https://git.sr.ht/~misterio/nix-config/tree/e2b8e924cfb7bb9f5d71e4e3e1fe92c181d46a65/item/hosts/common/optional/misterio-greetd.nix

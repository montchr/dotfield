# FIXME: currently results in a quick flash of lightdm (the default login
# manager on nixos) before attempting to boot into the initial session or
# loading the default session
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.dotfield.features) hasNvidia;

  swayFlags = lib.optionalString hasNvidia "--unsupported-gpu";
  mkSession = name: pkgs.writeShellScriptBin "dotfield-session-${name}";

  # If some users aren't able to shutdown/reboot, refer to the following:
  # https://github.com/apognu/tuigreet#power-management
  #
  # FIXME: swaynag options here are not keyboard-focusable! without a mouse,
  # that means there's no way to select them...
  sway-kiosk = command: ''
    ${pkgs.sway}/bin/sway ${swayFlags} --config \
      ${pkgs.writeText "kiosk.config" ''
        output * bg #000000 solid_color
        exec "${command}; ${pkgs.sway}/bin/swaymsg exit"

        bindsym Mod4+shift+e exec ${pkgs.sway}/bin/swaynag \
          -t warning \
          -m 'What do you want to do?' \
          -b 'Poweroff' 'systemctl poweroff' \
          -b 'Reboot' 'systemctl reboot'
      ''};
  '';

  greeterSession = sway-kiosk "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l";
  swaySession = mkSession "sway" ''
    ${pkgs.sway}/bin/sway ${swayFlags}
  '';
in
{
  environment.systemPackages = [
    pkgs.bashInteractive
    pkgs.fish
    swaySession
  ];

  environment.etc."greetd/environments".text = "${swaySession.name}\n" + "fish\n" + "bash\n";

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = greeterSession;
      };
      initial_session = lib.mkIf config.dotfield.guardian.enable (
        let
          user = config.dotfield.guardian.username;
          hmConfig = config.home-manager.users.${user};
        in
        {
          inherit user;
          command =
            if hmConfig.wayland.windowManager.sway.enable then
              swaySession.name
            else
              "${pkgs.fish}/bin/fish -l";
        }
      );
    };
  };
}
##: Sources
# - https://git.sr.ht/~misterio/nix-config/tree/e2b8e924cfb7bb9f5d71e4e3e1fe92c181d46a65/item/hosts/common/optional/misterio-greetd.nix

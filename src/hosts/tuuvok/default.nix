{
  config,
  moduleWithSystem,
  lib,
  self,
  ...
}:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.tuuvok = {
    system = "aarch64-linux";
    channel = "nixpkgs-apple-silicon";
    aspects = with config.aspects; [ workstation ];
    configuration = moduleWithSystem (
      perSystem@{ inputs', config }:
      {
        imports = [
          (nixos + "/mixins/jobwork.nix")
          # ./mixins/gnome.nix
          (nixos + "/mixins/niri.nix")
          (nixos + "/mixins/sway.nix")

          (nixos + "/profiles/hardware/apple/macbook-14-2/default.nix")
          (nixos + "/profiles/hardware/displaylink.nix")

          (nixos + "/profiles/remote-builders/default.nix")
          (nixos + "/profiles/remote-builders/ryosuke.nix")
        ];

        # By default, kanata captures all keyboard events, so most of the
        # configuration can be shared across every keyboard we use, by way of the
        # "default" configuration.  To prevent surprises, we specify devices
        # explicitly.
        services.kanata.keyboards."default".devices = [
          "/dev/input/by-path/platform-24eb30000.input-event-kbd"
          # Dell Smart Card Reader Keyboard KB813t (circa 2015)
          "/dev/input/by-id/usb-Dell_Dell_Smart_Card_Reader_Keyboard-event-kbd"
        ];

        security.pki.certificates = [
          # mkcert X.509 certificate for local development
          (builtins.readFile ./rootCA.crt)
        ];

        fonts.packages = [ perSystem.config.packages.berkeley-mono ];

        # NOTE: The firmware "asahi-tuuvok-firmware" repository results in
        # broken wifi.  Reverting to the "asahi-tuvok-firmware" repository works.
        hardware.asahi.peripheralFirmwareDirectory =
          perSystem.inputs'.asahi-tuvok-firmware.packages.default;

        time.timeZone = "America/New_York";

        dotfield.guardian.enable = true;
        dotfield.guardian.username = "cdom";
        users.mutableUsers = false;

        services.displayManager.autoLogin.enable = true;
        services.displayManager.autoLogin.user = "cdom";

        # <https://yalter.github.io/niri/Getting-Started.html#asahi-arm-and-other-kmsro-devices>
        # environment.etc."niri/config.kdl".text = ''
        #   debug {
        #       render-drm-device "/dev/dir/renderD128"
        #   }
        # '';

        services.avahi.enable = lib.mkForce false;
        # Not allowed because I don't want to make the building's network
        # switch mad again.
        services.avahi.publish.enable = false;

        services.tailscale.enable = true;

        sops.defaultSopsFile = ./secrets/secrets.yaml;

        system.stateVersion = "23.11"; # Did you read the comment?
      }
    );
  };
}

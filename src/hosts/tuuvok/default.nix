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
    aspects = with config.aspects; [
      hardware__apple__macbook-14-2
      workstation
      desktop-sessions__niri
      desktop-sessions__sway
      development__kleinweb
      hardware__displaylink
      remote-builds__ryosuke
    ];
    configuration = moduleWithSystem (
      perSystem@{ inputs', config }:
      {
        security.pki.certificates = [
          # mkcert X.509 certificate for local development
          (builtins.readFile ./rootCA.crt)
        ];

        fonts.packages = [ perSystem.config.packages.berkeley-mono ];

        # NOTE: The firmware "asahi-tuuvok-firmware" repository results in
        # broken wifi.  Reverting to the "asahi-tuvok-firmware" repository works.
        hardware.asahi.peripheralFirmwareDirectory =
          perSystem.inputs'.asahi-tuuvok-firmware.packages.default;

        time.timeZone = "America/New_York";

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

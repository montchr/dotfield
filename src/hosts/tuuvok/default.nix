{
  lib,
  config,
  inputs,
  ...
}:
let
  hostName = "tuuvok";
in
{
  dotfield.hosts.nixos.${hostName} = {
    modules = with config.dotfield.modules; [
      jobwork
      sway
      workstation

      "greeter/regreet"
      "hardware/apple-macbook-14-2"

      # DisplayLink is currently required for Asahi monitor support.
      # Asahi does not yet support standard DP-Alt display output.
      # DP-Alt output is required for true HDMI or DP output via one
      # of this machine's two USB-C ports and zero HDMI/DP ports.
      # DisplayLink provides a proprietary video-over-USB protocol
      # requiring special external hardware, either a dock or a
      # monitor natively supporting DisplayLink, but for now, I'll
      # take it.
      "hardware/displaylink"
    ];

    imports = [
      inputs.nixos-apple-silicon.nixosModules.apple-silicon-support
    ];

    nixpkgs.overlays = lib.mkBefore [ inputs.nixos-apple-silicon.overlays.default ];
  };
}

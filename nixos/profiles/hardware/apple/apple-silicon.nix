{
  lib,
  flake,
  ...
}:
let
  inherit (flake.inputs) nixos-apple-silicon;
in
{
  imports = [
    nixos-apple-silicon.nixosModules.apple-silicon-support

    ../../boot/systemd-boot.nix
  ];

  # HACK: <https://github.com/tpwrules/nixos-apple-silicon/issues/248>
  systemd.package =
    (import flake.inputs.nixpkgs-systemd-boot {
      system = "aarch64-linux";
    }).systemd;

  boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  # Mutually exclusive legacy Apple hardware.
  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;
}

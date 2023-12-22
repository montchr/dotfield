# NOTE: See also the related sharedProfile.
{
  lib,
  pkgs,
  flake,
  ...
}: let
  inherit (flake.inputs) nix-index-database;
in {
  imports = [nix-index-database.nixosModules.nix-index];

  # NOTE: Manpage cache generation may add significant time to builds.
  # FIXME: cannot set to false without conflict! even with mkDefault
  documentation.man.generateCaches = lib.mkDefault true;

  programs.htop.enable = true;
  programs.mtr.enable = true;

  # Nix-oriented package search tool and `command-not-found` replacement.
  #
  # `nix-index` is useful in itself, but fish shell *needs* it, as
  # `command-not-found` simply spits out errors.
  #
  # <https://github.com/nix-community/nix-index>
  #
  programs.command-not-found.enable = false;
  programs.nix-index.enable = true;
  # NOTE: This will, perhaps presumptuously, install `comma`.
  programs.nix-index-database.comma.enable = true;

  # TODO: audit these -- the list originally came from linode's recommended
  # tools for their support staff
  environment.systemPackages = with pkgs; [
    dosfstools
    gptfdisk
    inetutils
    iputils
    lm_sensors # <- standard tool for temperature monitoring <https://hwmon.wiki.kernel.org/lm_sensors>
    pciutils
    sysstat
    usbutils
    util-linux
  ];
}

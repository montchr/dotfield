# SPDX-FileCopyrightText: 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

flake@{ self, inputs, ... }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  snapshotPath = "/etc/nixos";
  remoteUrl = "git@codeberg.org:montchr/dotfield.git";
in
{
  networking.hostName = "installer";

  # The ISO builds its own bootloader; the core aspect's systemd-boot would
  # otherwise try to install to a nonexistent ESP.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  sops.age.sshKeyPaths = lib.mkForce [ ];
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = lib.mkForce "prohibit-password";
    settings.PasswordAuthentication = lib.mkForce false;
  };
  users.users.root.openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
  users.users.nixos.openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;

  hardware.enableRedistributableFirmware = true;
  networking.networkmanager.enable = true;

  environment.systemPackages = [
    inputs.disko.packages.${pkgs.stdenv.hostPlatform.system}.default
  ]
  ++ (with pkgs; [
    btrfs-progs
    cryptsetup
    dosfstools
    e2fsprogs
    emacs-pgtk
    firefox
    gparted
    gptfdisk
    just
    neovim
    nvme-cli
    parted
    pciutils
    rsync
    smartmontools
    tmux
    usbutils
  ]);

  # An offline fallback: a reinstall should not depend on the network or on
  # the operator's key being available to clone.
  environment.etc.nixos.source = self.outPath;

  users.motd = ''
    Dotfield installer

      flake snapshot : ${snapshotPath}
                       (offline, pinned at ISO build time)
      upstream       : ${remoteUrl}
                       (clone for the current config)

    Reinstall a host:

      disko-install --flake ${snapshotPath}#<host> --disk main /dev/disk/by-id/<id>
  '';

  image.fileName = "dotfield-installer-${config.system.nixos.label}-${pkgs.stdenv.hostPlatform.system}.iso";
  isoImage.volumeID = "DOTFIELD_ISO";

  system.stateVersion = "26.05";
}

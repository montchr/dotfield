# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2020-2021 Jörg Thalheim and nixos-shell contributors
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/Mic92/nixos-shell/blob/55de7d4d449ff30cdde8b8fe484a86eef477245e/share/modules/nixos-shell-config.nix
{
  config,
  lib,
  primaryUser,
  ...
}: let
  cfg = config.nixos-vm;
  mkVMDefault = lib.mkOverride 900;
in {
  virtualisation.vmVariant = {
    lib,
    pkgs,
    ...
  }: {
    imports = [
      ./vm-filesystems.nix
      # TODO: nothing here yet
      # ./vm-networking.nix
    ];

    # Kind of hacky way to answer the question "are we in a VM?"
    nixos-vm.enable = true;

    # Preserve most of the host machine's peer config but override networking.
    nixos-vm.peerConfig =
      lib.mkDefault
      ((lib.our.peers.getHost config.networking.hostName)
        // {
          network = "local";
          tailscale = null;
          ipv4 = null;
          ipv6 = null;
        });

    # Prefer a graphical QEMU window if the display server is enabled.
    virtualisation.graphics = mkVMDefault config.services.xserver.enable;

    # Distinguish the guest system's hostname from the host system's hostname.
    # Especially important for workstations where document/data sync may occur.
    # For example, while Syncthing would (likely) be able to distinguish between
    # each node with a UUID, it would be confusing and chaotic to have multiple
    # nodes default to the same name.
    networking.hostName = lib.mkVMOverride cfg.hostName;

    networking.firewall.enable = mkVMDefault false;
    services.openssh.enable = mkVMDefault true;

    users.mutableUsers = false;

    # Allow the user to login as root without password.
    users.extraUsers.root.initialHashedPassword = lib.mkVMOverride "";
    users.users.root.openssh.authorizedKeys.keys = lib.mkVMOverride primaryUser.authorizedKeys;

    services.getty.helpLine = lib.mkVMOverride ''
      Log in as "root" with an empty password.
      If you are connected via serial console:
      Type Ctrl-a c to switch to the qemu console
      and `quit` to stop the VM.
    '';

    environment.systemPackages = with pkgs; [
      bat
      fd
      git
      htop
      ripgrep
      vim
      xterm # for resize command
    ];
  };
}

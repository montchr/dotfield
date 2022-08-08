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
  inherit (config.networking) hostName;
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

    # FIXME: does not read from the TOML peer configs since this isn't a real
    # hostname. peers needs some kind of fallback logic, or perhaps more
    # appropriate, should be overridden or translated for virtualisation
    # networking.hostName = lib.mkVMOverride "${hostName}-dev";
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

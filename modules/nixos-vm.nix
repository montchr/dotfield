# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2020-2021 Jörg Thalheim and nixos-shell contributors
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/Mic92/nixos-shell/blob/55de7d4d449ff30cdde8b8fe484a86eef477245e/share/modules/nixos-shell.nix
{
  config,
  lib,
  options,
  pkgs,
  modulesPath,
  ...
}: let
  cfg = config.nixos-vm;
  user = builtins.getEnv "USER";
in {
  options.nixos-vm = {
    enable = lib.mkEnableOption "Whether to enable VM-specific configuration.";

    hostName = lib.mkOption {
      type = lib.types.str;
      default = "${config.networking.hostName}-dev";
      description = "Hostname for the virtual machine.";
    };

    peerConfig = lib.mkOption {
      type = lib.types.attrs;
      default = null;
      description = "Override the virtual machine's peer ops configuration.";
    };

    # FIXME: must be created manually before the VM boots / shared directories mount!
    dataHome = lib.mkOption {
      type = lib.types.str;
      # FIXME: assumes `/persist/vm` exists -- can this be stored in the
      # user's home directory or some other common path accessible to the
      # user?
      # default = "/persist/vm/${config.system.name}/data";
      # FIXME: hardcoded user path, yuck
      default = "/home/seadoom/.local/share/vms/${config.system.name}/data";
      description = "Directory on the host machine where the VM's data will live.";
    };

    mounts = {
      mountHome = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to mount <filename>/home</filename>.";
      };

      # FIXME: this won't work in pure eval mode
      mountNixProfile = lib.mkOption {
        type = lib.types.bool;
        # FIXME: should be true
        default = false;
        description = "Whether to mount the user's nix profile.";
      };

      extraMounts = lib.mkOption {
        inherit
          (options.virtualisation.sharedDirectories)
          default
          description
          example
          type
          ;
      };
    };
  };
}

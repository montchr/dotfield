{
  inputs,
  config,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) qemuArch;
  inherit (l.options) mkEnableOption mkOption;
  l = inputs.nixpkgs.lib // builtins;
  t = l.types;
  cfg = config.nix.nixos-builder-vm;
in {
  options.nix.nixos-builder-vm = {
    enable = mkEnableOption "NixOS VM guest builder";
    hostName = mkOption {
      type = t.str;
      default = "localhost";
      description = ''
        Hostname for the guest machine.

        Unless you have configured an alias in your host machine's SSH config,
        this should remain at its default value to reflect upstream.

        Maps to `nix.buildMachines.*.hostName`.
      '';
    };
    system = mkOption {
      type = t.str;
      default = "${qemuArch}-linux";
      description = ''
        Emulated system of the QEMU guest machine.

        In most cases, the guest and host architectures should match.
        That is, if the host is an Apple Silicon processor (i.e. `aarch64-darwin`),
        the guest system string should be `aarch64-linux`.

        Maps to `nix.buildMachines.*.system`.
      '';
    };
    maxJobs = mkOption {
      type = t.int;
      default = 4;
      description = ''
        The number of concurrent jobs to allow in the guest builder.

        Maps to `nix.buildMachines.*.maxJobs`.
      '';
    };
    useSubstitutes = mkOption {
      type = t.bool;
      default = false;
      description = ''
        Whether the guest builder should use substitutes during all builds.
        This will reduce the amount of storage required by the guest machine.
        Results in adding `builders-use-substitues = true` to `/etc/nix/nix.conf.`
      '';
    };
  };
  config = l.mkIf cfg.enable {
    nix = {
      buildMachines = [
        (import ./machine.nix {
          inherit (cfg) hostName maxJobs system;
        })
      ];
      distributedBuilds = true;
      # FIXME: when the guest builder is available, it will output warnings for
      # any host-platform-targeted builds run on the host. maybe ensure the
      # guest builder is only used explicitly? i rarely need to build for
      # `aarch64-linux` anyway.
      extraOptions = ''
        builders-use-substitutes = true
      '';
    };
  };
}

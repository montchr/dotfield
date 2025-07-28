{ lib, inputs, ... }:
{
  dotfield.modules.workstation.nixos = {
    programs.nh = {
      enable = true;
      # <https://github.com/viperML/nh/issues/88>
      flake = "/etc/nixos";
    };
  };

  dotfield.nixos =
    { config, pkgs, ... }:
    {
      imports = [ inputs.nix-index-database.nixosModules.nix-index ];

      environment.systemPackages = [ config.nix.package ];
      environment.etc =
        inputs
        |> lib.mapAttrs' (
          n: v: {
            name = "nix/inputs/${n}";
            value.source = v.outPath;
          }
        );

      nix = {
        nixPath = [
          "nixpkgs=${pkgs.path}"
          "home-manager=${inputs.home-manager}"
          "/etc/nix/inputs"
        ];
        distributedBuilds = true;
        allowed-users = [ "*" ];
        trusted-users = [
          "root"
          "@wheel"
        ];

        #### Nix Store Maintenance

        # By its very nature, optimizing during normal operations leads
        # to a drastic performance degradation.
        #
        # <https://github.com/NixOS/nix/issues/6033>
        # <https://discourse.nixos.org/t/should-i-enable-nix-autooptimisestore-on-my-laptop/12217/>
        settings.auto-optimise-store = false;
        # Instead, optimize on a timer.  By default, as of 2025-08-08,
        # this is set to run at 03:45.  But if the system is not powered
        # on at that time, optimisation will trigger on the next
        # wake/boot.  I recall running into issues with this on macOS in
        # the past.  Maybe... run it during lunch?
        optimise.automatic = true;
        optimise.dates = [ "12:30" ];

        gc.automatic = lib.mkDefault true;
        gc.dates = lib.mkDefault "weekly";

        #### Features

        experimental-features = [
          "flakes"
          "nix-command"
          "pipe-operators"
        ];

        # TODO: always appropriate??
        system-features = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];

        #### Substituters

        builders-use-substitutes = true;
        substituters = [
          "https://dotfield.cachix.org"
          "https://nix-community.cachix.org"
        ];
        trusted-substituters = [
          "ssh://eu.nixbuild.net"
          "https://nixpkgs-update.cachix.org/"
        ];
        trusted-public-keys = [
          "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
          "nixbuild.net/cdom-1:DU7hcG2k5kj9nC6NUvsOYQNiaI5UXYjjY5gBOccaND4="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8="
        ];
      };

      programs.nix-index.enable = true;
      # NOTE: This will install the `comma` command-line tool.
      programs.nix-index-database.comma.enable = true;

      # Nix-oriented package search tool and `command-not-found` replacement.
      #
      # `nix-index` is useful in itself, but fish shell *needs* it, as
      # `command-not-found` simply spits out errors.
      #
      # <https://github.com/nix-community/nix-index>
      #
      programs.command-not-found.enable = false;
    };
}

# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: 2022-2023 Lord-Valen
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
# Source: <https://github.com/Lord-Valen/configuration.nix/blob/2a0472a06c82e7e358822a74756b7c36b6fe2755/comb/repo/devshells.nix>
{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;
in
  l.mapAttrs (_: inputs.std.lib.dev.mkShell) {
    hive = _: {
      name = "dotfield -- hive";
      commands = let
        inherit (inputs) nixos-generators;
        hex = attrset: attrset // {category = "hex";};
        # inherit (inputs) nixos-generators colmena;
      in [
        (hex {package = nixos-generators.packages.nixos-generate;})
        (hex {
          name = "spawn-larva";
          help = "Spawns larva, the x86_64-linux iso bootstrapper.";
          command = ''
            echo "Boostrap image is building..."
            if path=$(nix build $PRJ_ROOT#nixosConfigurations._queen-o-larva.config.system.build.isoImage --print-out-paths);
            then
               echo "Boostrap image build finished."
            else
               echo "Boostrap image build failed."
            fi
          '';
        })

        # (hex {package = colmena.packages.colmena;})

        {
          category = "nix";
          name = "switch";
          help = "Switch configurations";
          command = "sudo nixos-rebuild switch --flake $PRJ_ROOT $@";
        }
        {
          category = "nix";
          name = "boot";
          help = "Switch boot configuration";
          command = "sudo nixos-rebuild boot --flake $PRJ_ROOT $@";
        }
        {
          category = "nix";
          name = "test";
          help = "Test configuration";
          command = "sudo nixos-rebuild test --flake $PRJ_ROOT $@";
        }
        {
          category = "nix";
          name = "update";
          help = "Update inputs";
          command = "nix flake update $PRJ_ROOT $@";
        }
        {
          category = "nix";
          name = "check";
          help = "Check flake";
          command = "nix flake check $PRJ_ROOT $@";
        }
      ];
    };
  }

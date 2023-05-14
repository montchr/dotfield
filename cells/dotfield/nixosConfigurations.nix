# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.cells) substituters wayland;
  inherit (cell) nixosProfiles;
in {
  larva = {
    bee.system = "aarch64-linux";
    bee.pkgs = inputs.nixpkgs;
    imports = [nixosProfiles.bootstrap];
  };
  freundix = {
    bee.system = "aarch64-linux";
    bee.pkgs = inputs.nixpkgs;
    imports = [
      nixosProfiles.graphical
      substituters.profiles.nixpkgs-wayland
      wayland.nixosProfiles.default
      {
        nix.settings.trusted-substituters = ["https://nixpkgs-wayland.cachix.org"];
      }
    ];
    # modules = with roles; gnome ++ graphical;
  };
}

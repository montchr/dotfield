# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  # inherit (inputs.cells) substituters;
in {
  default = {
    # TODO: probably no longer necessary
    environment.variables.MOZ_ENABLE_WAYLAND = "1";

    environment.systemPackages = [
      nixpkgs.wl-clipboard
    ];
  };
}

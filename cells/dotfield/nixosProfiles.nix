# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: 2022-2023 Lord-Valen
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
# Source: <https://github.com/Lord-Valen/configuration.nix/blob/2a0472a06c82e7e358822a74756b7c36b6fe2755/comb/repo/nixosProfiles.nix>
{
  inputs,
  cell,
}: let
  inherit (inputs) cells;
  bootstrap = import ./nixosProfiles/bootstrap.nix;
in {
  inherit bootstrap;
  graphical = cells.graphical.nixosProfiles.default;
}

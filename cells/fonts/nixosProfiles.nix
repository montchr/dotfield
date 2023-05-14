# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.haumea.lib) load loaders;
  srcs = load {
    src = ./nixosProfiles;
    loader = loaders.verbatim;
  };
in {
  inherit (srcs) common fontconfig iosevka-variants;
  default = {
    _file = ./nixosProfiles.nix;
    imports = [
      srcs.common
      srcs.fontconfig
      srcs.iosevka-variants
    ];
  };
}

# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.cells) fonts graphical;
  inherit (inputs.haumea.lib) load loaders;
  srcs = load {
    src = ./nixosProfiles;
    loader = loaders.verbatim;
  };
in {
  inherit (srcs) common;
  default = {
    _file = ./nixosProfiles.nix;
    imports = [
      graphical.nixosProfiles.common
      fonts.nixosProfiles.common
      fonts.nixosProfiles.fontconfig
      fonts.nixosProfiles.iosevka-variants
    ];
  };
}

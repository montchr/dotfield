# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  flake,
  withSystem,
  ...
}:
let
  inherit (lib) mkOption types;

  flakeSpecialArgs = flake;
  flakeSpecialArgs' =
    system:
    withSystem system (
      { inputs', ... }@ctx:
      let
        perSystem = {
          inherit (ctx.config) legacyPackages packages;
          inherit inputs';
        };
      in
      flakeSpecialArgs // { inherit perSystem; }
    );
in
{
  inherit flakeSpecialArgs flakeSpecialArgs';

  moduleType =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

  collectNixosModules = modules: lib.foldr (v: acc: acc ++ v.nixos.imports) [ ] modules;

  collectHomeModules = modules: lib.fold (v: acc: acc ++ v.home.imports) [ ] modules;
}

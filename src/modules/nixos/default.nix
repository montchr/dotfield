# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-FileCopyrightText: (C) 2025-2026 Temple University <kleinweb@temple.edu>
# SPDX-License-Identifier: GPL-3.0-or-later

{ withSystem, ... }: {
  flake.modules.nixos = {
    # citrix-secure-access =
    #   { config, pkgs, ... }:
    #   {
    #     imports = [ ./citrix-secure-access/nixos-module.nix ];

    #     services.citrix-secure-access.package = withSystem pkgs.stdenv.hostPlatform.system (
    #       { config, ... }: config.packages.citrix-secure-access
    #     );
    #   };

    "hardware/keyboard/keyboardio" = ./hardware/keyboard/keyboardio/__module.nix;
  };
}

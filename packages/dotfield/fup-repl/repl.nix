# FIXME: error: attribute 'getFlake' missing
#
# Copyright (C) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0+
#
# Copyright (C) 2021 Gytis Ivaskevicius
# SPDX-License-Identifier: MIT
#
##: Sources:
#
# https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/2bf0f91643c2e5ae38c1b26893ac2927ac9bd82a/lib/repl.nix
#
##: Changelog:
#
# - [2022-09-14] Formatting with `alejandra`
# - [2022-09-14] Initial commit
{
  flakePath ? null,
  hostnamePath ? "/etc/hostname",
  registryPath ? /etc/nix/registry.json,
}: let
  inherit
    (builtins)
    getFlake
    head
    match
    currentSystem
    readFile
    pathExists
    filter
    fromJSON
    ;

  selfFlake =
    if pathExists registryPath
    then filter (it: it.from.id == "self") (fromJSON (readFile registryPath)).flakes
    else [];

  flakePath' =
    toString
    (
      if flakePath != null
      then flakePath
      else if selfFlake != []
      then (head selfFlake).to.path
      else "/etc/nixos"
    );

  flake =
    if pathExists flakePath'
    then getFlake flakePath'
    else {};
  hostname =
    if pathExists hostnamePath
    then head (match "([a-zA-Z0-9\\-]+)\n" (readFile hostnamePath))
    else "";

  nixpkgsFromInputsPath = flake.inputs.nixpkgs.outPath or "";
  nixpkgs =
    flake.pkgs.${currentSystem}.nixpkgs
    or (
      if nixpkgsFromInputsPath != ""
      then import nixpkgsFromInputsPath {}
      else {}
    );

  nixpkgsOutput = removeAttrs (nixpkgs // nixpkgs.lib or {}) ["options" "config"];
in
  {inherit flake;}
  // flake
  // builtins
  // (flake.nixosConfigurations or {})
  // flake.nixosConfigurations.${hostname} or {}
  // nixpkgsOutput
  // {getFlake = path: getFlake (toString path);}

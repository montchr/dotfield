# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText Copyright (C) 2019-2022 Robert Helgesson
# SPDX-License-Identifier: MIT
#
# https://gitlab.com/rycee/nur-expressions/-/blob/f3cd885477a561a8bdfd5814439e31c4882ab72d/pkgs/firefox-addons/default.nix
{
  fetchurl,
  lib,
  stdenv,
}:
lib.makeOverridable ({
    pname,
    version,
    addonId,
    url,
    sha256,
    meta,
    ...
  }:
    stdenv.mkDerivation {
      name = "${pname}-${version}";

      inherit meta;

      src = fetchurl {inherit url sha256;};

      preferLocalBuild = true;
      allowSubstitutes = true;

      buildCommand = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p "$dst"
        install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    })

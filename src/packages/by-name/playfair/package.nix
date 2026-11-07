# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  fetchFromGitHub,
  nix-update-script,
  installFonts,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "playfair";
  version = "2.203";
  __structuredAttrs = true;
  strictDeps = true;

  src = fetchFromGitHub {
    owner = "clauseggers";
    repo = "Playfair";
    tag = finalAttrs.version;
    hash = "sha256-CUVeppMt4W551g0uwdxOHCaWSirpJTEaDMfs21odpIU=";
  };

  outputs = [
    "out"
    "webfont"
  ];

  dontConfigure = true;
  dontBuild = true;

  passthru.updateScript = nix-update-script { };

  nativeBuildInputs = [ installFonts ];

  # installFonts adds a hook to `postInstall` that installs fonts
  # into the correct directories
  installPhase = ''
    runHook preInstall
    runHook postInstall
  '';

  meta = {
    description = "Playfair is a general purpose Open Source typeface family";
    homepage = "https://github.com/clauseggers/Playfair";
    changelog = "https://github.com/clauseggers/Playfair/releases/tag/${finalAttrs.src.tag}";
    license = lib.licenses.ofl;
    maintainers = with lib.maintainers; [ montchr ];
    platforms = lib.platforms.all;
  };
})

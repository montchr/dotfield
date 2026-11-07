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
  pname = "cormorant";
  version = "4.002";
  __structuredAttrs = true;
  strictDeps = true;

  src = fetchFromGitHub {
    owner = "CatharsisFonts";
    repo = "Cormorant";
    tag = "v${finalAttrs.version}";
    hash = "sha256-vYn6MV+P+YVH329NM9tfAsNG8bsgGTJtDLOgnNYRMFk=";
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
    description = "Cormorant open-source display font family";
    homepage = "https://github.com/CatharsisFonts/Cormorant";
    changelog = "https://github.com/CatharsisFonts/Cormorant/releases/tag/${finalAttrs.src.tag}";
    license = lib.licenses.ofl;
    maintainers = with lib.maintainers; [ montchr ];
    platforms = lib.platforms.all;
  };
})

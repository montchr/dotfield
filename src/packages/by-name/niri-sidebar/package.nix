# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  rustPlatform,
  fetchFromGitHub,
  nix-update-script,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "niri-sidebar";
  version = "0.4.0";
  __structuredAttrs = true;

  src = fetchFromGitHub {
    owner = "Vigintillionn";
    repo = "niri-sidebar";
    tag = "v${finalAttrs.version}";
    hash = "sha256-MYP1ZiwV9+yJhl0zpuri6NQkQHlaYZjGBhXpZEaPZyI=";
  };

  cargoHash = "sha256-zZlfwAxWE1ZZy6k7QoBOamCGigGShd89sD27vfvgR00=";

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "A lightweight, external sidebar manager for the Niri window manager";
    homepage = "https://github.com/Vigintillionn/niri-sidebar";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "niri-sidebar";
  };
})

# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: 2003-2022 Eelco Dolstra and the Nixpkgs/NixOS contributors
# SPDX-License-Identifier: GPL-3.0-or-later OR MIT
#
# TODO: remove after upstream PR is merged: https://github.com/NixOS/nixpkgs/pull/201549
{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "lint-staged";
  version = "13.0.3";

  src = fetchFromGitHub {
    owner = "okonet";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-gQrgDX0+fpLz4Izrw29ChwBUXXXrUyZqV7BWtz9Ru8k=";
  };

  npmDepsHash = "sha256-bBSM8jCPHqHkynayHslvjJhH7HshSrmp0O6RJNA0tdM=";

  dontNpmBuild = true;

  meta = with lib; {
    description = "Run linters on git staged files";
    homepage = "https://github.com/okonet/lint-staged";
    license = licenses.mit;
    maintainers = with maintainers; [montchr];
  };
}

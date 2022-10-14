# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# For unfortunate circumstances only.
#
# https://github.com/layfellow/cpanel-cli
{
  lib,
  python3Packages,
  source,
}: let
  cpanel-api = python3Packages.buildPythonPackage rec {
    pname = "cpanel-api";
    version = "0.3.0";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-2KHfhY/736fbxQ3CoKxikmm8xNjgydkDH+R4V33vl4Y=";
    };
    propagatedBuildInputs = with python3Packages; [requests];
    doCheck = true;
  };
in
  python3Packages.buildPythonPackage rec {
    inherit (source) pname version src;
    propagatedBuildInputs = with python3Packages; [
      cpanel-api
      parsedatetime
      requests
    ];
    doCheck = true;
    meta = with lib; {
      description = "A command line interface for the cPanel Unrestricted API.";
      homepage = "https://github.com/layfellow/cpanel-cli";
      license = licenses.gpl3;
      maintainers = with maintainers; [montchr];
    };
  }

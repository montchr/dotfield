# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2022 GuangTao Zhang
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/GTrunSec/nixos-flk/blob/96ce0881a2185261758c0ad206d4149ad47d5b04/pkgs/python/promnesia/default.nix
# https://github.com/karlicoss/promnesia/issues/137
{
  stdenv,
  python3Packages,
  python3,
  fetchFromGitHub,
  orgparse,
  hpi,
  sources,
}: let
  mistletoe = python3Packages.buildPythonPackage rec {
    pname = "mistletoe";
    version = "0.7.2";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "18z6hqfnfjqnrcgfgl5pkj9ggf9yx0yyy94azcn1qf7hqn6g3l14";
    };
    doCheck = false;
    nativeBuildInputs = with python3Packages; [];
    propagatedBuildInputs = with python3Packages; [];
  };

  hug = python3Packages.buildPythonPackage rec {
    inherit (sources.hug) pname version src;
    doCheck = false;
    nativeBuildInputs = with python3Packages; [pytestrunner];
    propagatedBuildInputs = with python3Packages; [falcon requests];
    postPatch = ''
      substituteInPlace setup.py \
          --replace "falcon==2.0.0" "falcon"
    '';
  };

  cachew = python3Packages.buildPythonPackage rec {
    pname = "cachew";
    version = "0.8.0";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-0GkXoCPOnt43Ef+rCe0aPsRHVRaq3oSjHdwXw50rLuU=";
    };
    doCheck = false;
    propagatedBuildInputs = with python3Packages; [
      sqlalchemy
      setuptools_scm
    ];
  };

  urlextract = python3Packages.buildPythonPackage rec {
    pname = "urlextract";
    version = "1.0.0";
    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1mlgiixxlidqgmm8z2dkxahr4dx5s0m9fgp65z8nj70h2f7y4z6d";
    };
    doCheck = false;
    propagatedBuildInputs = with python3Packages; [
      appdirs
      dnspython
      uritools
      idna
      filelock
    ];
  };
in
  python3Packages.buildPythonPackage rec {
    inherit (sources.promnesia) pname version src;
    makeWrapperArgs = ["--prefix PYTHONPATH : $PYTHONPATH"];

    postPatch = ''
      substituteInPlace setup.py \
          --replace "idna<3" "idna" \
          --replace "tzlocal>=3.0" "tzlocal"
    '';

    SETUPTOOLS_SCM_PRETEND_VERSION = version;

    doCheck = false;

    propagatedBuildInputs = with python3Packages; [
      pytest
      appdirs
      python_magic
      hug
      tzlocal
      cachew
      urlextract
      orgparse
      logzero
      markdown
      lxml
      beautifulsoup4
      hpi
      mistletoe
    ];
  }

# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2022 GuangTao Zhang
# SPDX-License-Identifier: MIT
#
# https://github.com/GTrunSec/nixos-flk/blob/96ce0881a2185261758c0ad206d4149ad47d5b04/pkgs/python/orgparse/default.nix
{
  lib,
  python3Packages,
}:
python3Packages.buildPythonPackage rec {
  pname = "orgparse";
  version = "0.3.1";
  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-hg5vu5pnt0K6p5LmD4zBhSLpeJwGXSaCHAIoXV/BBK8=";
  };
  propagatedBuildInputs = with python3Packages; [setuptools_scm];
  doCheck = false;
  meta = with lib; {
    description = "Python module for reading Emacs org-mode files";
    homepage = "https://github.com/karlicoss/orgparse";
    license = licenses.asl20;
    maintainers = with maintainers; [montchr];
  };
}

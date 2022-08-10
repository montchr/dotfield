# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2022 GuangTao Zhang
# SPDX-License-Identifier: MIT
#
# https://github.com/GTrunSec/nixos-flk/blob/96ce0881a2185261758c0ad206d4149ad47d5b04/pkgs/python/HPI/default.nix
{
  lib,
  python3Packages,
  fetchurl,
  sources,
}:
python3Packages.buildPythonPackage rec {
  inherit (sources.HPI) pname version src;

  propagatedBuildInputs = with python3Packages; [
    pandas
    appdirs
    more-itertools
    mypy
    decorator
    setuptools_scm
    click
  ];

  makeWrapperArgs = ["--prefix PYTHONPATH : $PYTHONPATH"];

  doCheck = false;

  meta = with lib; {
    description = "Human Programming Interface 🧑👽🤖";
    homepage = "https://github.com/karlicoss/HPI";
    license = licenses.asl20;
    maintainers = with maintainers; [montchr];
  };
}

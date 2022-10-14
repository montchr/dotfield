# SPDX-FileCopyrightText Copyright (c) 2003-2022 Eelco Dolstra and the Nixpkgs/NixOS contributors
# SPDX-License-Identifier: MIT
#
# FIXME: runtime failure: "ModuleNotFoundError: No module named 'archivebox.vendor.atomicwrites'"
#
##: Source:
#
# https://github.com/NixOS/nixpkgs/blob/2ad6a705e163639af4646e8232bd33d86288a648/pkgs/applications/misc/archivebox/default.nix
{
  lib,
  fetchFromGitHub,
  python3Packages,
}:
python3Packages.buildPythonPackage {
  pname = "archivebox";
  version = "dev";

  src = fetchFromGitHub {
    owner = "ArchiveBox";
    repo = "ArchiveBox";
    rev = "03eb7e58758d8dcb85ed781e713fc083f8292264";
    sha256 = "sha256-15e5yBG4FtTekOY8V4njIDtz+PgaPu0xs719fafEchE=";
  };

  propagatedBuildInputs = with python3Packages; [
    requests
    mypy-extensions
    django
    django-extensions
    dateparser
    youtube-dl
    python-crontab
    croniter
    w3lib
    ipython
  ];

  postPatch = ''
    substituteInPlace setup.py \
        --replace "django>=3.1.3,<3.2" "django"
    substituteInPlace setup.py \
        --replace "django-extensions>=3.0.3" "django-extensions"
  '';

  meta = with lib; {
    description = "Open source self-hosted web archiving";
    homepage = "https://archivebox.io";
    license = licenses.mit;
    maintainers = with maintainers; [siraben];
    platforms = platforms.unix;
  };
}

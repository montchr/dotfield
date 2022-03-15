{
  stdenv,
  pkgs,
  lib,
}: let
  inherit (stdenv) mkDerivation;
  inherit (pkgs) fetchzip php php80Packages;
  inherit (php80Packages) composer;

  pname = "phpactor";
  version = "1194ce124a358e115d8e88c9309367cd7acf62ba";
in
  mkDerivation {
    inherit pname version;

    src = fetchzip {
      url = "https://github.com/phpactor/phpactor/archive/${version}.zip";
      hash = "sha256-qus7cwafQ7YJoeHnHHEk2Jv0HbfZi7AEmL0x3XQteIw=";
    };

    phases = ["unpackPhase" "installPhase"];

    buildInputs = [
      php
      composer
    ];

    installPhase = ''
      cp -r $src $out
    '';

    meta = with lib; {
      description = "PHP completion, refactoring, introspection tool and language server.";
      license = licenses.mit;
      homepage = "https://phpactor.readthedocs.io/";
      maintainers = with maintainers; [montchr];
    };
  }

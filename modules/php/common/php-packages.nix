{composerEnv, fetchurl, fetchgit ? null, fetchhg ? null, fetchsvn ? null, noDev ? false}:

let
  packages = {
    "squizlabs/php_codesniffer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "squizlabs-php_codesniffer-74ab905e9debf6ad05ea8eca6e456a3aa913d448";
        src = fetchurl {
          url = "https://api.github.com/repos/squizlabs/PHP_CodeSniffer/zipball/74ab905e9debf6ad05ea8eca6e456a3aa913d448";
          sha256 = "06r42ljsa1npbjsi7kc6zni0z7jkdppz1iwkig8xrq3iisaqmh70";
        };
      };
    };
    "svanderburg/composer2nix" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "svanderburg-composer2nix-9983c6fafb277f6b305edf232c2690d6d28ec092";
        src = fetchurl {
          url = "https://api.github.com/repos/svanderburg/composer2nix/zipball/9983c6fafb277f6b305edf232c2690d6d28ec092";
          sha256 = "1s1gv2b4y9pjv56mif8fgch56sssdmrcwb1gk3gksxc0jq2w2zbv";
        };
      };
    };
    "svanderburg/pndp" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "svanderburg-pndp-4bfe9c4120c23354ab8dc295957dc3009a39bff0";
        src = fetchurl {
          url = "https://api.github.com/repos/svanderburg/pndp/zipball/4bfe9c4120c23354ab8dc295957dc3009a39bff0";
          sha256 = "0n2vwpwshv16bhb7a6j95m664zh4lpfa7dqmcyhmn89nxpgvg91y";
        };
      };
    };
  };
  devPackages = {};
in
composerEnv.buildPackage {
  inherit packages devPackages noDev;
  name = "montchr-dotfield-common";
  src = ./.;
  executable = false;
  symlinkDependencies = false;
  meta = {
    license = "GPL-3.0-or-later";
  };
}

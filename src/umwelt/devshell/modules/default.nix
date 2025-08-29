{
  name,
  config,
  lib,
  pkgs,
}:
let
  inherit (lib) mkEnableOption mkOption types;

  envVarSubmodule = {
    options = {
      name = mkOption {
        type = types.str;
      };

      value = mkOption {
        type =
          with types;
          (oneOf [
            str
            int
            bool
            package
          ]);
        default = null;
      };

      eval = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "\${XDG_CONFIG_HOME:-\${HOME}/.config}";
      };

      unset = mkEnableOption "Whether to unset this variable rather than set its value";

      __toString = mkOption {
        type = types.functionTo types.str;
        internal = true;
        readOnly = true;
        default = unsafeEnvToBash;
        description = "Function used to translate this submodule to *unsafe* Bash code";
      };
    };
  };

  unsafeEnvToBash =
    {
      name,
      value,
      eval,
      unset,
      ...
    }@args:
    let
      vals = lib.filter (key: args.${key} != null && args.${key} != false) [
        "eval"
        "prefix"
        "unset"
        "value"
      ];
      valType = head vals;
    in
    assert lib.assertMsg (
      (lib.length vals) > 0
    ) "[[umwelt]]: ${name} expected one of (value|eval|prefix|unset) to be set.";
    assert lib.assertMsg ((lib.length vals) < 2)
      "[[umwelt]]: ${name} expected only one of (value|eval|prefix|unset) to be set. Not ${toString vals}";
    assert lib.assertMsg (
      !(name == "PATH" && valType == "value")
    ) "[[umwelt]]: ${name} should not override the value. Use 'prefix' instead.";
    if valType == "value" then
      "export ${name}=${lib.escapeShellArg (toString value)}"
    else if valType == "eval" then
      "export ${name}=${eval}"
    else if valType == "prefix" then
      ''export ${name}=$(${pkgs.coreutils}/bin/realpath --canonicalize-missing "${lib.prefix}")''${${name}+:''${${name}}}''
    else if valType == "unset" then
      ''unset ${name}''
    else
      throw "BUG in the core Umwelt module.  Please file a bug report.";

in
{
  options = {
    name = mkOption {
      type = types.str;
      default = name;
    };
    packages = mkOption {
      type = types.listOf types.package;
      default = [ ];
    };
    env = mkOption {
      type = types.listOf (types.submodule envVarSubmodule);
      example = lib.literalExpression ''
        [
          {
            name = "HTTP_PORT";
            value = 8080;
          }
          {
            name = "API_KEY";
            eval = "pass show api.fnord.com/api-key";
          }
          {
            name = "XDG_CACHE_HOME";
            eval = "''${PRJ_ROOT}/.cache";
          }
          {
            name = "CARGO_HOME";
            unset = true;
          }
        ]
      '';
    };
    shellHook = mkOption {
      type = types.lines;
      default = "";
    };
    finalPackage = mkOption {
      type = types.package;
      readOnly = true;
      internal = true;
    };
  };

  config = {
    finalPackage = pkgs.stdenv.mkDerivation (finalAttrs: {
      inherit (pkgs.mkShell { }) preferLocalBuild phases buildPhase;
      nativeBuildInputs = config.packages;
      shellHook = ''
        unset NIX_BUILD_TOP NIX_BUILD_CORES NIX_STORE
        unset TEMP TEMPDIR TMP TMPDIR
        unset builder out shellHook stdenv system
        unset dontAddDisableDepTrack outputs

        export PRJ_ROOT=$PWD

        export UMWELT_DEVSHELL_DIR=$out

        export PATH=$UMWELT_DEVSHELL_DIR/bin:$PATH


        ${config.shellHook}
      '';
      meta = config.meta;
    });
  };
}

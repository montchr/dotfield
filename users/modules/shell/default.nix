{ lib, config, options, pkgs, ... }:

let
  inherit (pkgs.lib.our) mkOpt mkOpt';

  cfg = config.shell;

  aliasLines = lib.mapAttrsToList
    (n: v: ''alias ${n}="${v}"'')
    (cfg.abbrs // cfg.aliases);
in

{
  options = {
    shell = with lib.types; {

      abbrs = mkOpt' (attrsOf str) { } ''
        Similar to aliases, but intended for inline expansion. For example, fish
        shell has abbreviations as a core feature.
      '';

      aliases = mkOpt (attrsOf str) { };

      rcInit = mkOpt' lines ''
        ${builtins.readFile ./rc-init.sh}
        ${lib.concatStringsSep "\n" aliasLines}
        ${lib.concatMapStrings (path: "source '${path}'") cfg.rcFiles}
      '' ''
        Lines to be sourced by login shell configuration
        files E.G. `.bashrc` and `.zshrc`
      '';
      rcFiles = mkOpt (listOf (either str path)) [ ];

      envInit = mkOpt' lines ''
        ${builtins.readFile ./env-init.sh}
        ${lib.concatMapStrings (path: "source '${path}'") cfg.envFiles}
      '' ''
        Lines to be sourced as early in the shell startup process as possible.
        In the case of ZSH, this will be sourced at every prompt, so keep it
        light!
      '';
      envFiles = mkOpt (listOf (either str path)) [ ];

    };
  };
}

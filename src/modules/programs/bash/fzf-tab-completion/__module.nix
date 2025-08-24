{ localFlake, withSystem }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types
    ;
  cfg = config.programs.bash.fzf-tab-completion;
in
{
  options.programs.bash.fzf-tab-completion = {
    enable = mkEnableOption "Whether to enable the fzf tab completion plugin for Bash";
    package = mkOption {
      default = withSystem pkgs.stdenv.hostPlatform.system (
        { config, ... }: config.packages.fzf-tab-completion
      );
      defaultText = lib.literalMD "`packages.fzf-tab-completion` from the dotfield flake";
      type = types.package;
    };
  };

  config = mkIf cfg.enable {
    programs.bash.initExtra = lib.mkAfter ''
      source ${cfg.package}/share/bash/fzf-bash-completion.sh
      bind -x '"\t": fzf_bash_completion'
    '';
  };
}

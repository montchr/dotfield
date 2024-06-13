{
  lib,
  flake,
  pkgs,
  ...
}:

{
  options.programs.zsh.grml = {
    enable = lib.mkEnableOption "grml-zsh-config";
  };

  config = {
    programs.zsh.initExtraFirst = flake.lib.zsh.mkInitConfigPreset ''
      source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';

    programs.starship.enableZshIntegration = false;
    programs.liquidprompt.enableZshIntegration = false;
  };
}

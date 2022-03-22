{ config, ... }: let

  in {
  home-manager.sharedModules = [
    {
      # home.sessionVariables = {
      #   inherit (config.environment.sessionVariables) NIX_PATH;
      # };
      xdg.enable = true;
      # xdg.configFile."nix/registry.json".text =
      #   config.environment.etc."nix/registry.json".text;
    }
  ];
}

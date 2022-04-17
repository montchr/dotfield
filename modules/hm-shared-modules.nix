{config, ...}: {
  home-manager.sharedModules = [
    {
      lib.dotfield.whoami = rec {
        firstName = "Chris";
        lastName = "Montgomery";
        fullName = "${firstName} ${lastName}";
        email = "chris@cdom.io";
        pgpPublicKey = "0x135EEDD0F71934F3";
      };
    }
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

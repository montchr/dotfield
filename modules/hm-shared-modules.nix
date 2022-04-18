{config, ...}: {
  home-manager.sharedModules = [
    {
      lib.dotfield.whoami = rec {
        firstName = "Chris";
        lastName = "Montgomery";
        fullName = "${firstName} ${lastName}";
        email = "chris@cdom.io";
        githubUserName = "montchr";
        pgpPublicKey = "0x135EEDD0F71934F3";
      };
    }
    {
      # TODO: is this the right place to define this?
      programs.home-manager.enable = true;

      # Necessary for home-manager to work with flakes, otherwise it will
      # look for a nixpkgs channel.
      home.stateVersion = "21.11";

      xdg.enable = true;

      # TODO: what benefit does symlinking this provide?
      # xdg.configFile."nix/registry.json".text =
      #   config.environment.etc."nix/registry.json".text;
    }
  ];
}

{
  withSystem,
  collective,
  self,
  ...
}: let
  inherit (self) inputs;
  inherit
    (inputs)
    nixpkgs
    nixos-stable
    nixos-unstable
    home-manager
    nix-colors
    ;
  inherit (collective) peers;
  inherit (inputs.digga.lib) flattenTree rakeLeaves;

  homeModules = flattenTree (rakeLeaves ./modules);
  homeProfiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit homeProfiles;};

  defaultProfiles = with homeProfiles; [
    core
    direnv
    navi
    nnn
    ranger
    secrets.common
    tealdeer
    vim
  ];

  defaultModules =
    defaultProfiles
    ++ (builtins.attrValues homeModules)
    ++ [
      ../lib/home
      {
        # _module.args.peers = peers;
        _module.args.self = self;
        _module.args.inputs = self.inputs;
        # _module.args.primaryUser = primaryUser;
      }
      nix-colors.homeManagerModule
      ({
        config,
        lib,
        pkgs,
        ...
      }: let
        inherit (config.home) username;
      in {
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;
        home.stateVersion = lib.mkDefault "22.05";
        # https://github.com/nix-community/home-manager/issues/2942
        nixpkgs.config.allowUnfreePredicate = pkg: true;

        home.homeDirectory =
          if pkgs.stdenv.hostPlatform.isDarwin
          then "/Users/${username}"
          else "/home/${username}";
      })
    ];

  extraSpecialArgs = {inherit homeProfiles roles;};

  makeHomeConfiguration = {
    username,
    modules ? [],
  }: (home-manager.lib.homeManagerConfiguration {
    inherit extraSpecialArgs;
    modules =
      defaultModules
      ++ modules
      ++ [{home.username = username;}];
  });

  traveller = makeHomeConfiguration {
    username = "cdom";
    modules = with roles; remote ++ webdev;
  };

  sharedConfiguration = {
    home-manager = {
      inherit extraSpecialArgs;
      sharedModules = defaultModules;
      useUserPackages = true;
      verbose = true;
    };
  };
in {
  flake.nixosModules.hm-shared-config = sharedConfiguration;
  flake.darwinModules.hm-shared-config = sharedConfiguration;
  flake.homeModules = homeModules;
  flake.homeConfigurations = {
    "cdom@kweb-prod-www" = traveller;
    "cdom@kweb-prod-db" = traveller;
    "cdom@kweb-dev" = traveller;
  };
}

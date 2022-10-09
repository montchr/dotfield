{
  self,
  withSystem,
  peers,
  ...
}: let
  inherit
    (self)
    inputs
    nixosConfigurations
    darwinConfigurations
    ;
  inherit
    (inputs.digga.lib)
    flattenTree
    mkHomeConfigurations
    rakeLeaves
    ;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;

  homeModules = flattenTree (rakeLeaves ./modules);
  homeProfiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit homeProfiles;};

  defaultProfiles = with homeProfiles; [
    core
    direnv
    navi
    nnn
    secrets.common
    tealdeer
    vim
  ];

  defaultModules =
    (builtins.attrValues homeModules)
    ++ defaultProfiles
    ++ [
      ../lib/home
      inputs.nix-colors.homeManagerModule
      ({
        config,
        lib,
        pkgs,
        ...
      }: let
        inherit (config.home) username;
        inherit (pkgs.stdenv.hostPlatform) isDarwin;
        homePrefix =
          if isDarwin
          then "/Users"
          else "/home";
      in {
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;
        home.stateVersion = lib.mkDefault "22.05";
        home.homeDirectory = "${homePrefix}/${username}";
      })
    ];

  extraSpecialArgs = {
    inherit
      homeProfiles
      roles
      ;
  };

  makeHomeConfiguration = username: hmArgs:
    withSystem (hmArgs.system or x86_64-linux) (
      ctx @ {...}: let
        pkgs = hmArgs.pkgs or ctx.pkgs;
        moduleArgs = {
          _module.args.self = self;
          _module.args.inputs = self.inputs;
          _module.args.packages = ctx.config.packages;
          _module.args.sources = ctx.sources;
          _module.args.peers = peers;
        };
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit extraSpecialArgs;
        modules =
          defaultModules
          ++ (hmArgs.modules or [])
          ++ [
            moduleArgs
            {home.username = username;}
          ];
      })
    );

  traveller = makeHomeConfiguration "cdom" {
    modules = with roles; remote ++ webdev;
  };

  settingsProfile = {...}: {
    home-manager = {
      inherit extraSpecialArgs;
      sharedModules = defaultModules;
      useGlobalPkgs = true;
      useUserPackages = true;
      verbose = true;
    };
  };
in {
  flake = {
    inherit homeModules;
    sharedProfiles.homeManagerSettings = settingsProfile;
    homeConfigurations =
      (mkHomeConfigurations nixosConfigurations)
      // (mkHomeConfigurations darwinConfigurations)
      // {
        "cdom@kweb-prod-www" = traveller;
        "cdom@kweb-prod-db" = traveller;
        "cdom@kweb-dev" = traveller;
      };
  };
}

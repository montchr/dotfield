{
  self,
  withSystem,
  peers,
  moduleWithSystem,
  lib,
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

  defaultModules =
    (builtins.attrValues homeModules)
    ++ (with homeProfiles; [
      core
      direnv
      navi
      nnn
      secrets.common
      tealdeer
      vim
    ])
    ++ [
      ../lib/home
      inputs.nix-colors.homeManagerModule
      (moduleWithSystem (perSystem @ {...}: {
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
        _module.args = {
          inherit self peers;
          inputs = self.inputs;
          sources = perSystem.sources;
          packages = perSystem.config.packages;
        };
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;
        home.stateVersion = lib.mkDefault "22.05";
        home.homeDirectory = "${homePrefix}/${username}";
      }))
    ];
  userDefaults = username: defaultModules ++ [{home.username = username;}];

  platformSpecialArgs = hostPlatform: {
    inherit homeProfiles roles;
    inherit (hostPlatform) isDarwin isLinux isMacOS system;
  };

  makeHomeConfiguration = username: args:
    withSystem (args.system or x86_64-linux) (
      perSystem @ {system, ...}: let
        inherit ((args.pkgs or perSystem.pkgs).stdenv) hostPlatform;
      in (inputs.home-manager.lib.homeManagerConfiguration {
        modules = (userDefaults username) ++ (args.modules or []);
        extraSpecialArgs = platformSpecialArgs hostPlatform;
      })
    );

  traveller = makeHomeConfiguration "cdom" {
    modules = with roles; remote ++ webdev;
  };

  settingsModule = moduleWithSystem (perSystem: osArgs: let
    inherit ((osArgs.pkgs or perSystem.pkgs).stdenv) hostPlatform;
  in {
    home-manager = {
      extraSpecialArgs = platformSpecialArgs hostPlatform;
      sharedModules = defaultModules;
      useGlobalPkgs = true;
      useUserPackages = true;
      verbose = true;
    };
  });
in {
  flake = {
    inherit homeModules;
    nixosModules.homeManagerSettings = settingsModule;
    darwinModules.homeManagerSettings = settingsModule;
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

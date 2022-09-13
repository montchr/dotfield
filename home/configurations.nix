{
  self,
  withSystem,
  peers,
  ...
}: let
  inherit (self) inputs;
  inherit (inputs.digga.lib) flattenTree rakeLeaves;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;

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
      in {
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;
        home.stateVersion = lib.mkDefault "22.05";

        home.homeDirectory =
          if pkgs.stdenv.hostPlatform.isDarwin
          then "/Users/${username}"
          else "/home/${username}";
      })
    ];

  extraSpecialArgs = {
    inherit
      self
      inputs
      homeProfiles
      roles
      ;
  };

  makeHomeConfiguration = {
    username,
    system ? x86_64-linux,
    pkgs ? (withSystem system (ctx @ {...}: ctx.pkgs)),
    modules ? [],
  }: (withSystem system (
    ctx @ {...}: let
      moduleArgs = {
        _module.args.inputs = self.inputs;
        _module.args.packages = ctx.config.packages;
        _module.args.sources = ctx.sources;
        _module.args.peers = peers;
      };
    in (
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs extraSpecialArgs;
        modules =
          defaultModules
          ++ modules
          ++ [
            moduleArgs
            {home.username = username;}
          ];
      }
    )
  ));

  traveller = makeHomeConfiguration {
    username = "cdom";
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
    inherit homeModules homeProfiles;
    sharedProfiles.homeManagerSettings = settingsProfile;
    homeConfigurations = {
      "cdom@kweb-prod-www" = traveller;
      "cdom@kweb-prod-db" = traveller;
      "cdom@kweb-dev" = traveller;
    };
  };
}

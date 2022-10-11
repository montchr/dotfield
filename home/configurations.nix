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
  inherit (lib) mkBefore;

  homeModules = flattenTree (rakeLeaves ./modules);
  homeProfiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit homeProfiles;};

  bootstrapProfile = moduleWithSystem (
    perSystem @ {...}: (
      {config, ...}: let
        inherit (config.home) username;
        inherit (perSystem.pkgs.stdenv.hostPlatform) isDarwin;
        homePrefix =
          if isDarwin
          then "/Users"
          else "/home";
      in {
        _module.args = {
          inherit peers;
          packages = perSystem.config.packages;
        };
        home.homeDirectory = "${homePrefix}/${username}";
      }
    )
  );

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
      bootstrapProfile
      inputs.nix-colors.homeManagerModule
    ];

  platformSpecialArgs = hostPlatform: {
    inherit
      self
      inputs
      homeProfiles
      roles
      ;
    inherit
      (hostPlatform)
      isDarwin
      isLinux
      isMacOS
      system
      ;
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
    homeConfigurations = mkBefore (
      (mkHomeConfigurations nixosConfigurations)
      // (mkHomeConfigurations darwinConfigurations)
    );
  };

  perSystem = ctx @ {pkgs, ...}: {
    homeConfigurations = let
      userDefaults = username:
        defaultModules
        ++ [{home.username = username;}];

      makeHomeConfiguration = username: args: let
        pkgs = args.pkgs or ctx.pkgs;
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = (userDefaults username) ++ (args.modules or []);
        extraSpecialArgs = platformSpecialArgs pkgs.stdenv.hostPlatform;
      });

      traveller = makeHomeConfiguration "cdom" {
        modules = with roles; remote ++ webdev;
      };
    in {
      inherit traveller;
    };
  };
}

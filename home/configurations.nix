{
  self,
  inputs,
  moduleWithSystem,
  # DEPRECATED:
  peers,
  primaryUser,
  ...
}: let
  inherit (self) nixosConfigurations darwinConfigurations;
  inherit (inputs.apparat.lib) flattenTree homePrefix;
  inherit (inputs.haumea.lib) load loaders;
  inherit (inputs.digga.lib) mkHomeConfigurations;
  l = inputs.nixpkgs.lib // builtins;

  homeModules = load {
    src = ./modules;
    loader = loaders.path;
  };

  profiles = load {
    src = ./profiles;
    loader = loaders.path;
  };

  roles = import ./roles.nix {inherit profiles;};

  defaultModules =
    (l.attrValues (flattenTree homeModules))
    ++ roles.base
    ++ [
      inputs.nix-colors.homeManagerModule
      (moduleWithSystem (
        {
          inputs',
          packages,
          ...
        }: args: {
          _module.args = {
            inherit
              inputs'
              packages
              peers
              primaryUser
              ;
          };
        }
      ))
    ];

  platformSpecialArgs = hostPlatform: {
    inherit
      self
      inputs
      profiles
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

  settingsModule = moduleWithSystem ({
    cells,
    pkgs,
    ...
  }: osArgs: let
    inherit ((osArgs.pkgs or pkgs).stdenv) hostPlatform;
  in {
    home-manager = {
      extraSpecialArgs = (platformSpecialArgs hostPlatform) // {inherit cells;};
      sharedModules = defaultModules;
      useGlobalPkgs = true;
      useUserPackages = true;
      # TODO
      # backupFileExtension = "bak";
    };
  });
in {
  flake = {
    inherit homeModules;
    nixosModules.homeManagerSettings = settingsModule;
    darwinModules.homeManagerSettings = settingsModule;
    homeConfigurations =
      (mkHomeConfigurations nixosConfigurations)
      // (mkHomeConfigurations darwinConfigurations);
  };

  perSystem = ctx @ {
    inputs',
    cells,
    system,
    ...
  }: {
    homeConfigurations = let
      makeHomeConfiguration = username: hmArgs: let
        inherit (pkgs.stdenv) hostPlatform;
        pkgs = hmArgs.pkgs or ctx.pkgs;
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          defaultModules
          ++ [
            (moduleArgs: {
              home.username = username;
              home.homeDirectory = "${homePrefix system}/${username}";
              _module.args = {
                inherit cells inputs';
                isNixos =
                  (moduleArgs.nixosConfig ? hardware)
                  # We only care if the option exists -- its value doesn't matter.
                  && (moduleArgs.nixosConfig.hardware.enableRedistributableFirmware -> true);
              };
            })
          ]
          ++ (hmArgs.modules or []);
        extraSpecialArgs = platformSpecialArgs hostPlatform;
      });

      traveller = makeHomeConfiguration "cdom" {
        modules = roles.remote ++ roles.webdev;
      };
    in {
      inherit traveller;
    };
  };
}

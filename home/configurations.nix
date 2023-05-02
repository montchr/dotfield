{
  self,
  moduleWithSystem,
  # DEPRECATED:
  peers,
  primaryUser,
  ...
}: let
  inherit
    (self)
    inputs
    nixosConfigurations
    darwinConfigurations
    ;
  inherit (inputs.apparat.lib) tern;
  inherit (inputs.apparat.lib.tree) flatten rake;
  inherit (inputs.digga.lib) mkHomeConfigurations;
  l = inputs.nixpkgs.lib // builtins;

  homeModules = flatten (rake ./modules);
  profiles = rake ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultModules =
    (l.attrValues homeModules)
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
    };
  });
in {
  flake = {
    inherit homeModules;
    nixosModules.homeManagerSettings = settingsModule;
    darwinModules.homeManagerSettings = settingsModule;
    homeConfigurations = l.mkBefore (
      (mkHomeConfigurations nixosConfigurations)
      // (mkHomeConfigurations darwinConfigurations)
    );
  };

  perSystem = ctx @ {
    inputs',
    cells,
    ...
  }: {
    homeConfigurations = let
      makeHomeConfiguration = username: hmArgs: let
        inherit (pkgs.stdenv) hostPlatform;
        inherit (hostPlatform) isDarwin;
        pkgs = hmArgs.pkgs or ctx.pkgs;
        homePrefix = tern isDarwin "/Users" "/home";
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          defaultModules
          ++ [
            (moduleArgs: {
              home.username = username;
              home.homeDirectory = "${homePrefix}/${username}";
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
        modules = with roles; remote ++ webdev;
      };
    in {
      inherit traveller;
    };
  };
}

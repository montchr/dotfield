{
  self,
  lib,
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
  inherit (inputs.nix-std.lib.bool) ifThenElse;
  inherit
    (inputs.digga.lib)
    flattenTree
    mkHomeConfigurations
    rakeLeaves
    ;
  l = inputs.nixpkgs.lib // builtins;

  homeModules = flattenTree (rakeLeaves ./modules);
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultModules =
    (l.attrValues homeModules)
    # TODO: declare this in a real role for easier finding
    ++ (with profiles; [
      core
      direnv
      navi
      nnn
      nvim
    ])
    ++ [
      inputs.nix-colors.homeManagerModule
      (moduleWithSystem (
        ctx @ {inputs', ...}: args: {
          _module.args = {
            inherit inputs' peers primaryUser;
            inherit (ctx.config) packages;
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

  settingsModule = moduleWithSystem ({pkgs, ...}: osArgs: let
    inherit ((osArgs.pkgs or pkgs).stdenv) hostPlatform;
  in {
    home-manager = {
      extraSpecialArgs = platformSpecialArgs hostPlatform;
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

  perSystem = ctx @ {inputs', ...}: {
    homeConfigurations = let
      makeHomeConfiguration = username: hmArgs: let
        inherit (pkgs.stdenv) hostPlatform;
        inherit (hostPlatform) isDarwin;
        pkgs = hmArgs.pkgs or ctx.pkgs;
        homePrefix = ifThenElse isDarwin "/Users" "/home";
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          defaultModules
          ++ [
            (moduleArgs: {
              home.username = username;
              home.homeDirectory = "${homePrefix}/${username}";
              _module.args.inputs' = inputs';
              _module.args.isNixos =
                (moduleArgs.nixosConfig ? hardware)
                # We only care if the option exists -- its value doesn't matter.
                && (moduleArgs.nixosConfig.hardware.enableRedistributableFirmware -> true);
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

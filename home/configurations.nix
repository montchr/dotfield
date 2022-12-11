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
  inherit (lib) mkBefore;

  homeModules = flattenTree (rakeLeaves ./modules);
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  moduleArgs = moduleWithSystem (
    ctx @ {pkgsets, ...}: args: {
      _module.args = {
        inherit peers primaryUser pkgsets;
        inherit (ctx.config) packages;
      };
    }
  );

  defaultModules =
    (builtins.attrValues homeModules)
    # TODO: declare this in a real role for easier finding
    ++ (with profiles; [
      core
      direnv
      navi
      nnn
      nvim
    ])
    ++ [
      moduleArgs
      inputs.nix-colors.homeManagerModule
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

  settingsModule = moduleWithSystem ({pkgsets, ...}: osArgs: let
    inherit ((osArgs.pkgs or pkgsets.default).stdenv) hostPlatform;
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
    homeConfigurations = mkBefore (
      (mkHomeConfigurations nixosConfigurations)
      // (mkHomeConfigurations darwinConfigurations)
    );
  };

  perSystem = {pkgsets, ...}: {
    homeConfigurations = let
      makeHomeConfiguration = username: args: let
        inherit (pkgs.stdenv) hostPlatform;
        inherit (hostPlatform) isDarwin;
        pkgs = args.pkgs or pkgsets.default;
        homePrefix = ifThenElse isDarwin "/Users" "/home";
      in (inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          defaultModules
          ++ [
            (moduleArgs: {
              home.username = username;
              home.homeDirectory = "${homePrefix}/${username}";
              _module.args.isNixos =
                (moduleArgs.nixosConfig ? hardware)
                # We only care if the option exists -- its value doesn't matter.
                && (moduleArgs.nixosConfig.hardware.enableRedistributableFirmware -> true);
            })
          ]
          ++ (args.modules or []);
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

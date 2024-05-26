{
  ops,
  super,
  flake,
  withSystem,
  ...
}:
let
  inherit (super.modules) flakeSpecialArgs flakeSpecialArgs';
  inherit (flake.inputs) apparat haumea home-manager;
  inherit (apparat.lib) homePrefix;

  homeModules = import "${flake.self}/home/modules-list.nix";

  specialArgs = {
    inherit features profiles;
    flake = flakeSpecialArgs;
  };
  specialArgs' = system: specialArgs // { flake = flakeSpecialArgs' system; };

  settings = {
    extraSpecialArgs = specialArgs;
    sharedModules = defaultModules;
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  settings' = system: settings // { extraSpecialArgs = specialArgs' system; };

  defaultModules =
    homeModules
    ++ features.base
    ++ [
      {
        _module.args = {
          inherit ops;
        };
      }
    ];
  features = import ../../home/features.nix { inherit profiles; };
  profiles = import ../../home/profiles.nix { inherit haumea; };
in
{
  inherit defaultModules settings settings';

  makeHomeConfiguration =
    username: args:
    let
      inherit (args) pkgs;
      # FIXME: should be hostPlatform?
      inherit (pkgs.stdenv) system;
    in
    withSystem system (
      { pkgs, ... }:
      (home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = specialArgs' system;
        modules =
          defaultModules
          ++ (args.modules or [ ])
          ++ [
            {
              home.username = username;
              home.homeDirectory = "${homePrefix system}/${username}";
            }
          ];
      })
    );
}

{
  ops,
  super,
  flake,
  withSystem,
  ...
}: let
  inherit (super.modules) flakeSpecialArgs flakeSpecialArgs';
  inherit (flake.inputs) apparat haumea home-manager;
  inherit (apparat.lib) homePrefix;

  # TODO: is `outPath` necessary? added it during debugging
  homeModules = import "${flake.self.outPath}/home/modules-list.nix";
  homeProfiles = import "${flake.self.outPath}/profiles/homeProfiles.nix" {inherit haumea;};
  homeSuites = import "${flake.self.outPath}/profiles/homeSuites.nix" {inherit homeProfiles;};

  specialArgs = {
    flake = flakeSpecialArgs;
    profiles = homeProfiles;
    roles = homeSuites;
  };
  specialArgs' = system: specialArgs // {flake = flakeSpecialArgs' system;};

  settings = {
    extraSpecialArgs = specialArgs;
    sharedModules = defaultModules;
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  settings' = system: settings // {extraSpecialArgs = specialArgs' system;};

  defaultModules =
    homeModules
    ++ homeSuites.base
    ++ [{_module.args = {inherit ops;};}];
in {
  inherit defaultModules settings settings';

  makeHomeConfiguration = username: args: let
    inherit (args) pkgs;
    # FIXME: should be hostPlatform?
    inherit (pkgs.stdenv) system;
  in (withSystem system (
    {pkgs, ...}: (home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = specialArgs' system;
      modules =
        defaultModules
        ++ (args.modules or [])
        ++ [
          {
            home.username = username;
            home.homeDirectory = "${homePrefix system}/${username}";
          }
        ];
    })
  ));
}

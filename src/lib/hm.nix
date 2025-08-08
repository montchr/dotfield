{
  ops,
  super,
  flake,
  withSystem,
  ...
}:
let
  inherit (super.modules) flakeSpecialArgs flakeSpecialArgs';
  inherit (flake.inputs) apparat home-manager;
  inherit (apparat.lib) homePrefix;

  homeModules = import "${flake.self}/home/modules-list.nix";

  specialArgs = {
    flake = flakeSpecialArgs;
  };
  specialArgs' =
    system:
    specialArgs
    // {
      flake = flakeSpecialArgs // (flakeSpecialArgs' system);
    };

  settings = {
    extraSpecialArgs = specialArgs;
    sharedModules = defaultModules;
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  settings' = system: settings // { extraSpecialArgs = specialArgs' system; };

  defaultModules = homeModules ++ [
    ../../home/profiles/core/default.nix
    ../../home/profiles/direnv.nix
    ../../home/profiles/fzf.nix
    ../../home/profiles/git/default.nix
    {
      _module.args = {
        # FIXME: include self.lib somehow (will cause infinite recursion
        # if added in the //src/lib tree)
        inherit ops;
      };
    }
  ];
in
{
  inherit defaultModules settings settings';

  makeHomeConfiguration =
    username: args:
    let
      # FIXME: should use hostPlatform?
      system = args.pkgs.stdenv.system or args.system;
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

{
  self,
  inputs,
  withSystem,
  ...
}:
let
  inherit (self.lib.modules) flakeSpecialArgs flakeSpecialArgs';
  inherit (inputs) apparat home-manager;
  inherit (apparat.lib) homePrefix;

  homeModules = import "${self}/home/modules-list.nix";

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
    # Prevent activation failures by specifying how to handle file
    # collisions.  Just back them up, don't freak out.
    backupFileExtension = "bak";
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
  ];
in
{
  flake.lib.hm = {
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
  };
}

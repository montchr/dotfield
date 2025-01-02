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
    ../../home/profiles/atuin.nix
    ../../home/profiles/development/nix-tools.nix
    ../../home/profiles/direnv.nix
    ../../home/profiles/fzf.nix
    ../../home/profiles/git/default.nix
    ../../home/profiles/helix.nix
    ../../home/profiles/neovim/default.nix
    ../../home/profiles/nnn.nix
    ../../home/profiles/rclone.nix
    ../../home/profiles/shells/zsh/default.nix
    ../../home/profiles/shells/zsh/with-grml.nix
    ../../home/profiles/ssh.nix
    ../../home/profiles/zellij.nix
    ../../home/profiles/zoxide.nix

    {
      _module.args = {
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

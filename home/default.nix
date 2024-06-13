{
  self,
  inputs,
  flake-parts-lib,
  withSystem,
  ...
}:
let
  inherit (inputs) home-manager;
  inherit (flake-parts-lib) importApply;

  initUserModule =
    { username }:
    { lib, ... }:
    {
      # TODO: imports = [ ../users/${username}/identity.nix ];

      home.username = username;
      home.homeDirectory = lib.mkDefault "/home/${username}";
    };

  initNixpkgsModule = importApply ../packages/nixpkgs-config.nix;

  makeHomeConfiguration =
    username: system:
    {
      modules ? [ ],
      overlays ? [ ],
      allowUnfree ? false,
    }:
    (home-manager.lib.homeManagerConfiguration {
      pkgs = withSystem system (ctx: ctx.pkgs);
      modules =
        modules
        ++ (import ./modules-list.nix)
        ++ (import ./baseline.nix)
        ++ [
          (initUserModule { inherit username; })
          (initNixpkgsModule { inherit allowUnfree overlays; })
        ];
      extraSpecialArgs = (self.lib.specialArgsFor system) // {
        scoped.modulesPath = builtins.toString ./modules;
        scoped.profilesPath = builtins.toString ./profiles;
      };
    });
in
{
  flake = {
    homeConfigurations = {
      "cdom@tuvok" = makeHomeConfiguration "cdom" "aarch64-linux" {
        modules = [ ../users/cdom/at-tuvok.nix ];
      };
    };

    homeModules = {
      "theme" = import ./modules/theme/default.nix;
      "whoami" = import ./modules/dotfield/whoami.nix;
      "programs/bash/trampoline" = import ./modules/programs/bash/trampoline.nix;
      "programs/cod" = import ./modules/programs/cod.nix;
      "programs/liquidprompt" = import ./modules/programs/liquidprompt.nix;
    };
  };
}

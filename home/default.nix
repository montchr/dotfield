{
  self,
  withSystem,
  ops,
  flake-parts-lib,
  ...
}:
let
  inherit (self) inputs;
  inherit (inputs) haumea home-manager;
  inherit (flake-parts-lib) importApply;

  specialArgsFor = import ../lib/special-args.nix { inherit self withSystem; };

  makeHomeConfiguration =
    username: system:
    {
      modules ? [ ],
      overlays ? [ ],
    }:
    (home-manager.lib.homeManagerConfiguration {
      modules = modules ++ [ (import ./baseline.nix { inherit username system; }) ];
      extraSpecialArgs = specialArgsFor system;
      pkgs = import inputs.nixpkgs {
        inherit system overlays;
        nixpkgs.config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
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

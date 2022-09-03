{
  withSystem,
  collective,
  self,
  ...
}: let
  inherit (collective) peers;
  inherit (self.inputs) home-manager nix-colors nixos-unstable;
  inherit (self.inputs.digga.lib) flattenTree rakeLeaves;
  inherit (self.inputs.flake-utils.system) x86_64-linux;

  homeModules = builtins.attrValues self.homeManagerModules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultProfiles = with profiles; [
    core
    direnv
    navi
    nnn
    ranger
    secrets.common
    tealdeer
    vim
  ];

  defaultModules =
    defaultProfiles
    ++ homeModules
    ++ [
      {
        imports = [../lib/home];
        _module.args = {
          inherit
            peers
            profiles
            roles
            ;
        };
      }
      nix-colors.homeManagerModule
    ];

  makeHomeConfiguration = {
    username,
    system ? x86_64-linux,
    modules ? [],
  }: (home-manager.lib.homeManagerConfiguration {
    pkgs = nixos-unstable.legacyPackages.${system};
    modules =
      defaultModules
      ++ modules
      ++ [
        ./nixpkgs-config.nix
        ({
          pkgs,
          lib,
          ...
        }: {
          home.username = username;
          home.homeDirectory =
            if pkgs.stdenv.hostPlatform.isDarwin
            then "/Users/${username}"
            else "/home/${username}";
          home.stateVersion = lib.mkDefault "22.05";
        })
      ];
  });

  traveller = makeHomeConfiguration {
    username = "cdom";
    modules = with roles; remote ++ webdev;
  };
in {
  flake.homeModules = flattenTree (rakeLeaves ./home/modules);
  flake.homeConfigurations = {
    "cdom@kweb-prod-www" = traveller;
    "cdom@kweb-prod-db" = traveller;
    "cdom@kweb-dev" = traveller;
  };
}

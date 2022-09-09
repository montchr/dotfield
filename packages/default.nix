{
  self,
  config,
  lib,
  withSystem,
  ...
}: let
  inherit
    (builtins)
    functionArgs
    intersectAttrs
    isPath
    mapAttrs
    ;
in {
  config = {
    flake.overlays = {
      default = final: prev: self.packages;
    };

    perSystem = ctx @ {
      pkgs,
      config,
      inputs',
      ...
    }: let
      inherit (pkgs) callPackage;

      ##: nvfetcher sources
      #
      # some of these packages depend on raw sources managed by nvfetcher.
      # nvfetcher's output will not pass checks as a package, but its generated
      # nix expression still depends on various core fetcher packages so it
      # needs to have access to `pkgs`.
      #
      # at the time of writing, nvfetcher still seems like the simplest approach
      # to managing raw sources centrally (as opposed to a slew of scattered
      # dependency pinning throughout package expressions). even though flakes
      # seem like they'd provide a built-in solution to such a workflow, i gave
      # up after hours of trying to replace nvfetcher with a flakes-only
      # workflow (using a subflake in `./sources/flake.nix` to avoid cluttering
      # the toplevel inputs). if you, dear reader, have any pointers toward such
      # a workflow, i welcome your advice!
      sources =
        (import ./sources/_sources/generated.nix)
        {inherit (pkgs) fetchurl fetchgit fetchFromGitHub;};

      packageIndex = {
        ##: internal packages
        dotfield-config = ./dotfield/dotfield-config.nix;

        ##: application helpers
        firefox-lepton-ui = {source}: source.src;
        kitty-set-app-icon = ./applications/kitty/set-app-icon.nix;
        kitty-get-window-by-platform-id = ./applications/kitty/get-window-by-platform-id.nix;

        ##: development tools
        ediff-tool = ./development-tools/ediff-tool;
        git-submodule-rewrite = ./development-tools/git-submodule-rewrite;

        ##: drivers
        epson-201212w = ./drivers/epson_201212w;

        ##: fonts
        nerdfonts-symbols-only = ./fonts/nerdfonts-symbols-only.nix;
        # pragmatapro = ./fonts/pragmatapro.nix;
        sf-pro = ./fonts/sf-pro.nix;

        ##: golang packages
        trellis-cli = ./golang/trellis-cli;

        ##: php packages
        phpactor = ./php/phpactor;

        ##: python packages
        cpanel-cli = ./python/cpanel-cli;
        hpi = ./python/HPI;
        orgparse = ./python/orgparse;
        promnesia = ./python/promnesia;
      };

      packageDeps = name: package: (intersectAttrs
        (functionArgs package)
        (config.packages
          // {
            inherit (inputs'.gitignore.lib) gitignoreSource;
            source = sources.${name};
          }));
    in {
      _module.args.sources = sources;

      packages = mapAttrs (name: v: let
        package =
          if (isPath v)
          then (import v)
          else v;
      in (callPackage package (packageDeps name package)))
      packageIndex;
    };
  };
}

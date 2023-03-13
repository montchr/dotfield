{inputs, ...}: let
  inherit (inputs.flake-utils.lib) filterPackages flattenTree;
in {
  perSystem = ctx @ {
    pkgs,
    system,
    ...
  }: {
    _module.args.packages = ctx.config.packages;
    packages = filterPackages system (flattenTree {
      cod = pkgs.callPackage ./tools/misc/cod/cod.nix {};
      epson-201212w = pkgs.callPackage ./drivers/epson_201212w {};
      ddi = pkgs.callPackage ./tools/ddi.nix {};
      firefox-ui-fix = pkgs.callPackage ./applications/firefox/firefox-ui-fix/package.nix {};
      igr = pkgs.callPackage ./tools/igr.nix {};
      fre = pkgs.callPackage ./tools/misc/fre {};
      kitty-get-window-by-platform-id = pkgs.callPackage ./applications/kitty/get-window-by-platform-id {};
      sf-pro = pkgs.callPackage ./fonts/sf-pro.nix {};

      zsh-autocomplete = pkgs.zsh-autocomplete.overrideAttrs (_o: {
        version = inputs.zsh-autocomplete.rev;
        src = inputs.zsh-autocomplete;
      });
      zsh-completions = pkgs.zsh-completions.overrideAttrs (_o: {
        version = inputs.zsh-completions.rev;
        src = inputs.zsh-completions;
      });
    });
  };
}

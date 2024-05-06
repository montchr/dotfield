{ flake, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
  plugin = pkg: { inherit (pkg) name src; };
in
{
  imports = [
    ../common.nix
    ./__fzf-integration.nix
  ];

  programs.fish = {
    enable = true;
    plugins = l.map plugin (
      with pkgs.fishPlugins;
      [
        autopair
        done
        # TODO: will conflict with our shell aliases, needs configuration
        # forgit
      ]
    );
  };

  programs.fzf.enableFishIntegration = l.mkDefault true;
  programs.neovim.plugins = with pkgs.vimPlugins; [ vim-fish ];
}

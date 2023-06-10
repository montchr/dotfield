{
  config,
  pkgs,
  flake,
  ...
}: let
  inherit (flake.perSystem) packages;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) xdg;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./settings.nix
    ./symbols.nix
  ];

  home.packages = [
    packages.kitty-get-window-by-platform-id
  ];

  home.sessionVariables = {
    KITTY_CONFIG_DIRECTORY = "${xdg.configHome}/kitty";
  };

  programs.kitty = {
    enable = true;
    darwinLaunchOptions = l.mkIf isDarwin ["--single-instance"];
    keybindings = {
      # Open new tabs+windows from the current session's working directory.
      "kitty_mod+t" = "new_tab_with_cwd";
      "kitty_mod+enter" = "new_window_with_cwd";
    };
  };
}

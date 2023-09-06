moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isMacOS;

  cfg = config.programs.nnn;

  kittyCfg = config.programs.kitty;

  isGraphical = isMacOS || (moduleArgs.osConfig.services.xserver.enable or false);
  enablePreviews = config.programs.tmux.enable || kittyCfg.enable;

  shellAliases = {
    nnn = "${cfg.package}/bin/nnn -a";
  };

  previewDeps = with pkgs;
    [
      bat
      eza
      file
      man
      mediainfo
      pistol
      unzip
    ]
    ++ (lib.optionals (isGraphical && !isMacOS) [
      imagemagick
      ffmpeg
      ffmpegthumbnailer
      fontpreview
      poppler # pdf rendering
      viu
      w3m # text-mode web browser
    ]);
in {
  programs.nnn = {
    enable = true;
    plugins.src = "${pkgs.nnn.src}/plugins";
    plugins.mappings = {
      p = "preview-tui";
    };
    extraPackages =
      lib.optionals enablePreviews previewDeps;
  };

  home.sessionVariables = {
    NNN_PREVIEWDIR = "${config.xdg.cacheHome}/nnn/previews";
    # USE_PISTOL = lib.optionalString enablePreviews "1";
  };

  programs.bash = {inherit shellAliases;};
  programs.fish = {inherit shellAliases;};
  programs.zsh = {inherit shellAliases;};
}

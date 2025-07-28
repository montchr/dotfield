{
  dotfield.home =
    { config, pkgs, ... }:
    {
      programs.nnn = {
        enable = true;
        plugins.src = "${pkgs.nnn.src}/plugins";
      };

      home.sessionVariables = {
        NNN_FIFO = "/tmp/nnn.fifo";
        NNN_ICONLOOKUP = "1";
        NNN_PISTOL = "1";
        NNN_PREVIEWDIR = "${config.xdg.cacheHome}/nnn/previews";
      };

    };

  dotfield.modules.workstation.home =
    { pkgs, ... }:
    {
      programs.nnn.extraPackages = [
        pkgs.bat
        pkgs.eza
        pkgs.file
        pkgs.glow
        pkgs.man
        pkgs.mediainfo
        pkgs.pistol
        pkgs.unzip
        pkgs.imagemagick
        pkgs.ffmpeg
        pkgs.ffmpegthumbnailer
        pkgs.fontpreview
        pkgs.poppler # pdf rendering
        pkgs.viu
        pkgs.w3m # text-mode web browser
      ];
    };
}

{
  users.cdom.aspects.core.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      sessionVariables = {
        NNN_FIFO = "/tmp/nnn.fifo";
        NNN_ICONLOOKUP = "1";
        NNN_PISTOL = "1";
        NNN_PREVIEWDIR = "${config.xdg.cacheHome}/nnn/previews";
      };

      # FIXME: make sure the wrapped package can access these
      #        currently no syntax highlighting in previews
      previewDeps = [
        pkgs.bat
        pkgs.eza
        pkgs.file
        pkgs.glow
        pkgs.man
        pkgs.mediainfo
        pkgs.pistol
        pkgs.unzip
      ];

    in
    {
      programs.nnn = {
        enable = true;
        plugins.src = "${pkgs.nnn.src}/plugins";
        plugins.mappings = {
          p = "preview-tui";
        };
        extraPackages = previewDeps;
      };

      home = {
        inherit sessionVariables;
      };
      programs.bash = {
        inherit sessionVariables;
      };
      # FIXME: unsupported
      # programs.fish = {inherit sessionVariables;};
      # programs.nushell = {inherit sessionVariables;};
    };
}

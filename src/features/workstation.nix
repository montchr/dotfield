{
  aspects.workstation = {
    requires = [
      "development"
      "graphical"

      "hardware__epson__wf-3520"
      "hardware__yubico__yubikey"
    ];
    nixos =
      { pkgs, ... }:
      {
        networking.firewall =
          let
            kdeconnectPorts = {
              from = 1714;
              to = 1764;
            };
          in
          {
            allowedTCPPortRanges = [ kdeconnectPorts ];
            allowedUDPPortRanges = [ kdeconnectPorts ];
          };

        location.provider = "geoclue2";
        services.geoclue2.enable = true;

        services.dictd.enable = true;
        services.dictd.DBs = with pkgs.dictdDBs; [
          deu2eng
          eng2deu
          wiktionary
          wordnet
        ];

        programs.nh = {
          enable = true;
          # <https://github.com/viperML/nh/issues/88>
          flake = "/etc/nixos";
        };

        # NOTE: This will significantly slow down builds.  However, it enables more
        # manpage integrations across various tools (e.g. `apropos`, `man -k`).
        documentation.man.generateCaches = true;
      };

    home =
      { pkgs, ... }:
      {
        home.packages = [
          pkgs.kdePackages.okular
          pkgs.libreoffice-fresh
          pkgs.vscode
        ];

        programs.obs-studio.enable = true;

        services.git-sync.enable = true;
      };
  };
}

{
  aspects.workstation = {
    requires = [
      "development"
      "graphical"

      "hardware__epson__wf-3520"
      "hardware__focusrite__scarlett-18i20-mk1"
      "hardware__razer"
      "hardware__yubico__yubikey"
    ];

    nixos =
      { pkgs, ... }:
      {
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
          flake = "/etc/nixos";
        };
      };

    home =
      { pkgs, ... }:
      {
        home.packages = [
          pkgs.kdePackages.okular
          pkgs.libreoffice-fresh
          # TODO: ideally this would only be included at the
          # intersection of "graphical" and "mail" aspects, but there is
          # currently no mechanism to achieve that. (i'm thinking
          # module args / specialArgs of all active aspects...)
          pkgs.protonmail-bridge-gui
          pkgs.teams-for-linux
          pkgs.visidata
          pkgs.vscode
          pkgs.xlsx2csv
        ];
      };
  };
}

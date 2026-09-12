{ moduleWithSystem, ... }: {
  aspects.workstation = {
    requires = [
      "development"
      "graphical"

      "hardware__epson__wf-3520"
      "hardware__focusrite__scarlett-18i20-mk1"
      # FIXME: build failure on openrazer kernel module
      # "hardware__razer"
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
      };

    home = moduleWithSystem (
      perSystem@{ inputs' }:
      home@{ pkgs, ... }:
      {
        home.packages = [
          pkgs.ffmpeg
          pkgs.graphviz
          pkgs.kdePackages.okular
          pkgs.pdfarranger
          pkgs.python313Packages.weasyprint # archive webpages to PDF
          pkgs.texlivePackages.pdfbook2 # print PDF as booklet
          pkgs.unicode-character-database
          pkgs.visidata
          pkgs.vscode
          pkgs.xlsx2csv
        ];
      }
    );
  };
}

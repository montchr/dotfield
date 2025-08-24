{
  dotfield.aspects.workstation.nixos =
    { pkgs, ... }:
    {
      services.dictd.enable = true;
      services.dictd.DBs = with pkgs.dictdDBs; [
        deu2eng
        eng2deu
        wiktionary
        wordnet
      ];
    };
}

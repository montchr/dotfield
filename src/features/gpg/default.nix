{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        gnupg
        gpgme
        (writeShellScriptBin "gpg-agent-restart" ''
          pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
        '')
      ];

      programs.gpg = {
        enable = true;
        settings = {
          keyserver = "hkps://keys.openpgp.org";
          # keyserver = "hkps://keys.mailvelope.com";
          # FIXME: this should be (and, actually, is) the default
          # keyserver = "hkps://keyserver.ubuntu.com";
        };
      };
    };
}

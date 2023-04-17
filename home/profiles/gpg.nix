{
  config,
  pkgs,
  inputs,
  self,
  ...
}: let
  inherit (config.dotfield.whoami) pgpPublicKey;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
  keysDir = self + "/secrets/keys";
in {
  # Use our fork of these modules while still pending upstream changes.
  # FIXME: https://github.com/nix-community/home-manager/pull/2964
  imports = [(inputs.home-manager-gpg-agent-darwin + "/modules/services/gpg-agent.nix")];
  disabledModules = ["services/gpg-agent.nix"];

  config = l.mkIf ("" != pgpPublicKey) {
    home.sessionVariables.DOTFIELD_PGP_KEY = pgpPublicKey;

    home.packages = with pkgs; [
      gnupg
      gpgme

      (writeShellScriptBin "gpg-agent-restart" ''
        pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
      '')
    ];

    services.gpg-agent = {
      enable = true;
      enableSshSupport = false;
      pinentryFlavor = l.mkIf isDarwin "mac";
      sshKeys = [pgpPublicKey];
    };

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      # TODO: clean up the format/structure of these key files
      publicKeys = [
        {
          source = keysDir + "/${pgpPublicKey}.asc";
          trust = "ultimate";
        }
        {
          source = keysDir + "/0xF0B8FB42A7498482.asc";
          trust = "ultimate";
        }
      ];

      # https://github.com/drduh/config/blob/master/gpg.conf
      # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration-Options.html
      # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Esoteric-Options.html
      settings = {
        # Keyserver URL
        keyserver = "hkps://keys.openpgp.org";
        # keyserver hkps://keyserver.ubuntu.com:443
        # keyserver hkps://hkps.pool.sks-keyservers.net
        # keyserver hkps://pgp.ocf.berkeley.edu
      };
    };
  };
}

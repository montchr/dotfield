# TODO: backups <https://nixos-mailserver.readthedocs.io/en/latest/options.html#mailserver-borgbackup>
# TODO: newsletter list config e.g. List-Unsubscribe headers
{
  flake,
  ops,
  config,
  ...
}: let
  inherit (flake.inputs) simple-nixos-mailserver;
  inherit (ops.metadata.networks) loopgarden seadome;
  inherit (config.sops) secrets;
in {
  imports = [simple-nixos-mailserver.nixosModules.default];

  mailserver = {
    enable = true;
    fqdn = "mail.${loopgarden.domain}";
    domains = [loopgarden.domain];
    loginAccounts."hierophant@loop.garden" = {
      hashedPasswordFile = secrets."mailserver/accounts/hierophant/hashed-password".path;
      aliases = ["postmaster@loop.garden" "abuse@loop.garden"];
    };
    certificateScheme = "acme-nginx";
    # hierarchySeparator = "/";
    dmarcReporting.enable = true;
    dmarcReporting.domain = loopgarden.domain;
    dmarcReporting.organizationName = "Seadome Systems";
  };

  # Mailserver accounts correspond to system user accounts.
  sops.secrets."mailserver/accounts/hierophant/hashed-password".neededForUsers = true;
  sops.secrets."mailserver/accounts/testacct/hashed-password".neededForUsers = true;

  security.acme.acceptTerms = true;
  security.acme.defaults.email = seadome.contact;
}

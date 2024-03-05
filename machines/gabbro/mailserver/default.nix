# TODO: backups <https://nixos-mailserver.readthedocs.io/en/latest/options.html#mailserver-borgbackup>
{
  flake,
  ops,
  config,
  ...
}: let
  inherit (flake.inputs) simple-nixos-mailserver;
  inherit (ops.networks) seadome;
  inherit (config.sops) secrets;
  inherit (ops.networks.loopgarden) domain;
  matrixFqdn = "matrix.${domain}";
in {
  imports = [simple-nixos-mailserver.nixosModules.default];

  mailserver = {
    enable = true;
    fqdn = "mail.${domain}";
    domains = [domain matrixFqdn];

    ##: loop.garden
    loginAccounts."hierophant@${domain}" = {
      hashedPasswordFile = secrets."accounts/hierophant/hashed-password".path;
      catchAll = [domain];
    };
    loginAccounts."dmarc@${domain}".hashedPasswordFile = secrets."accounts/dmarc/hashed-password".path;

    ##: matrix.loop.garden
    loginAccounts."notifications@${matrixFqdn}" = {
      hashedPasswordFile = secrets."accounts/matrix/notifications/hashed-password".path;
      sendOnly = true;
    };
    loginAccounts."support@${matrixFqdn}" = {
      hashedPasswordFile = secrets."accounts/matrix/support/hashed-password".path;
      catchAll = [matrixFqdn];
    };

    certificateScheme = "acme-nginx";
    # hierarchySeparator = "/";
    dmarcReporting.enable = true;
    dmarcReporting.domain = domain;
    dmarcReporting.organizationName = "Seadome Systems";

    extraVirtualAliases = {
      "postmaster@${domain}" = "hierophant@${domain}";
      "abuse@${domain}" = "hierophant@${domain}";
      "postmaster@${matrixFqdn}" = "support@${matrixFqdn}";
      "abuse@${matrixFqdn}" = "support@${matrixFqdn}";
    };
  };

  # Mailserver accounts correspond to system user accounts.
  sops.secrets."accounts/hierophant/hashed-password".neededForUsers = true;
  sops.secrets."accounts/dmarc/hashed-password".neededForUsers = true;

  sops.secrets."accounts/matrix/notifications/hashed-password".neededForUsers = true;
  sops.secrets."accounts/matrix/support/hashed-password".neededForUsers = true;

  security.acme.acceptTerms = true;
  security.acme.defaults.email = seadome.contact;
}

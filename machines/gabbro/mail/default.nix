{
  flake,
  ops,
  ...
}: let
  inherit (flake.inputs) simple-nixos-mailserver;
  inherit (ops.metadata.networks.loopgarden) domain;
in {
  imports = [simple-nixos-mailserver.nixosModules.default];

  mailserver = {
    enable = true;
    fqdn = "mail.${domain}";
    domains = [domain];
    loginAccounts."hierophant@${domain}" = {
      aliases = ["postmaster@${domain}"];
    };
  };

  sops.secrets."mailserver/accounts/hierophant/hashed-password" = {};
}

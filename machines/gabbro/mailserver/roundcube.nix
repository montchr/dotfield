# via <https://nixos-mailserver.readthedocs.io/en/latest/add-roundcube.html>
{ ops, config, ... }:
{
  services.roundcube = {
    enable = true;
    # NOTE: This does not necessarily need to be the same FQDN as the mailserver.
    #       But for convenience and discoverability, it is a sensible default.
    hostName = "mail.${ops.networks.loopgarden.domain}";
    extraConfig = ''
      # STARTTLS is needed for authn, so the FQDN must match the certificate.
      $config['smtp_server'] = "tls://${config.mailserver.fqdn}";
      $config['smtp_user'] = "%u";
      $config['smtp_pass'] = "%p";
    '';
  };

  # TODO: replace with srvos nginx mixin
  services.nginx.enable = true;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}

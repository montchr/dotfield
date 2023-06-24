{
  lib,
  config,
  # pkgs,
  ...
}: {
  services.tailscale.enable = true;
  services.tailscale.interfaceName = "ts0";
  services.tailscale.useRoutingFeatures = lib.mkDefault "client";

  # Add MagicDNS nameserver, in addition to any existing nameservers.
  # <https://tailscale.com/kb/1063/install-nixos/#using-magicdns>
  networking.nameservers = ["100.100.100.100"];

  networking.firewall.trustedInterfaces = [config.services.tailscale.interfaceName];
  networking.firewall.allowedUDPPorts = [config.services.tailscale.port];

  # Authenticate on startup.
  # FIXME: this will cause a lock-out since it needs user input...
  # via https://discourse.nixos.org/t/solved-possible-to-automatically-authenticate-tailscale-after-every-rebuild-reboot/14296
  # systemd.services.tailscale-autoconnect = {
  #   description = "Automatic connection to Tailscale";
  #   after = ["network-pre.target" "tailscale.service"];
  #   wants = ["network-pre.target" "tailscale.service"];
  #   wantedBy = ["multi-user.target"];
  #   serviceConfig.Type = "oneshot";
  #   script = ''
  #     echo "Waiting for tailscale.service start completion ..."
  #     sleep 5

  #     echo "Checking if already authenticated to Tailscale ..."
  #     status="$(${pkgs.tailscale}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
  #     if [[ $status = "Running" ]]; then
  #       echo "Already authenticated to Tailscale, exiting."
  #       exit 0
  #     fi

  #     echo "Authenticating with Tailscale ..."
  #     ${pkgs.tailscale}/bin/tailscale up
  #   '';
  # };
}

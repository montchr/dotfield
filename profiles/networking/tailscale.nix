{
  config,
  lib,
  pkgs,
  ...
}: {
  services.tailscale.enable = true;

  networking.firewall.trustedInterfaces = [config.services.tailscale.interfaceName];
  networking.firewall.allowedUDPPorts = [config.services.tailscale.port];
  networking.firewall.checkReversePath = "loose";

  # Authenticate on startup.
  # via https://discourse.nixos.org/t/solved-possible-to-automatically-authenticate-tailscale-after-every-rebuild-reboot/14296
  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";
    after = ["network-pre.target" "tailscale.service"];
    wants = ["network-pre.target" "tailscale.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "oneshot";
    script = with pkgs; ''
      echo "Waiting for tailscale.service start completion ..."
      sleep 5

      echo "Checking if already authenticated to Tailscale ..."
      status="$(${tailscale}/bin/tailscale status -json | ${jq}/bin/jq -r .BackendState)"
      if [[ $status = "Running" ]]; then
        echo "Already authenticated to Tailscale, exiting."
        exit 0
      fi

      echo "Authenticating with Tailscale ..."
      ${tailscale}/bin/tailscale up
    '';
  };
}

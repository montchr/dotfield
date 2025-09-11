{
  aspects.development.nixos =
    { pkgs, ... }:
    {
      # FIXME: do not expose to world!
      networking.firewall.allowedTCPPorts = [
        9003 # xdebug
      ];

      environment.systemPackages = [
        pkgs.ddev
        pkgs.docker-buildx
        pkgs.mkcert

        # <https://ddev.readthedocs.io/en/stable/users/install/docker-installation/#testing-and-troubleshooting-your-docker-installation>
        (pkgs.writeShellScriptBin "ddev-verify-installation" ''
          docker run --rm -t -p 80:80 -p 443:443 -v "//$PWD:/tmp/projdir" \
            busybox sh -c "echo ---- Project Directory \
          && ls /tmp/projdir"
        '')
      ];
    };
}

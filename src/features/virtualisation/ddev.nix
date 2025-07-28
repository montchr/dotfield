{ lib, ... }:
{
  dotfield.modules."virtualisation/ddev".nixos =
    { config, pkgs, ... }:
    let
      cfg = config.programs.ddev;
    in
    {
      options = {
        programs.ddev = {
          enable = lib.mkEnableOption "ddev";
          package = lib.mkPackageOption pkgs "ddev" { };
        };
      };

      config = lib.mkIf cfg.enable {
        # FIXME: do not expose to world!
        networking.firewall.allowedTCPPorts = [
          9003 # xdebug
        ];

        environment.systemPackages = [
          cfg.package

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
    };
}

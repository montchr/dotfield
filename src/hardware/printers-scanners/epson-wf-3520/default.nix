{
  # TODO:     don't bother with this unless on home network -- may cause shutdown
  # delays on university network?
  flake.modules.nixos.at-home =
    { pkgs, ... }:
    let
      ip = "192.168.1.192";
    in
    {
      hardware.sane.extraBackends = [
        pkgs.epkowa

        (pkgs.writeTextFile {
          name = "epkowa.conf";
          # TODO: verify how much of this is necessary?
          text = ''
            net ${ip} 1865
          '';
          destination = "/etc/sane.d/epkowa.conf";
        })
      ];

      # FIXME: remove? what happens if the port is provided? is the extra conf file
      # still needed?
      hardware.sane.netConf = ''
        ${ip}
      '';
    };

}

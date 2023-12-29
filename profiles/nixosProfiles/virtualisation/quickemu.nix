{
  config,
  pkgs,
  ...
}: {
  services.samba.enable = true;
  # services.samba.shares = [];
  environment.systemPackages = [
    pkgs.quickemu

    config.services.samba.package
  ];
}

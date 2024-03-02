# WARNING: conflicts with home-manager module when extra packages are specified
{
  flake,
  config,
  pkgs,
  ...
}: let
  cfg = config.services.emacs;
in {
  # services.emacs.enable = true;
  services.emacs.package = pkgs.emacs29-macport;
  # environment.systemPackages = [cfg.package];
}

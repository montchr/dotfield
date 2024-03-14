# WARNING: conflicts with home-manager module when extra packages are specified
{
  pkgs,
  ...
}:
{
  # services.emacs.enable = true;
  services.emacs.package = pkgs.emacs29-macport;
  # environment.systemPackages = [cfg.package];
}

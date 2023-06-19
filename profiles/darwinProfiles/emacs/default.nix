{
  flake,
  config,
  ...
}: let
  cfg = config.services.emacs;
in {
  services.emacs.enable = true;
  services.emacs.package = flake.perSystem.packages.emacs-plus-29;

  environment.systemPackages = [cfg.package];
}

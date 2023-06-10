{flake, ...}: {
  environment.systemPackages = [flake.perSystem.packages.emacs-plus-edge];
}

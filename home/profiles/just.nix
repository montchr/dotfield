{pkgs, ...}: {
  home.packages = [pkgs.just];
  # FIXME: the upstream zsh completion loading mechanism is broken. installing
  # the package normally works just fine (the module doesn't really do anything
  # special if `home.extraOutputsToInstall` is configured properly)
  programs.just.enable = false;
}

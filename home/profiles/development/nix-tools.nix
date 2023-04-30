{pkgs, ...}: {
  home.packages = [
    pkgs.nix-init #   <- generate nix package expressions from url
  ];
}

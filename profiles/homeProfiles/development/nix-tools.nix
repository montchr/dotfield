{pkgs, ...}: {
  home.packages = [
    pkgs.alejandra
    pkgs.nix-init #   <- generate nix package expressions from url
    pkgs.nix-melt #   <- flake.lock explorer
    pkgs.nixpkgs-review
    pkgs.nvd #        <- diff package changes between versions
  ];
}

{
  imports = [ ./__lib.nix ];

  dotfield.features.remote-builders.nixos = {
    nix.distributedBuilds = true;
  };
}

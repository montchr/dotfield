{
  imports = [ ./__lib.nix ];

  dotfield.aspects.remote-builders.nixos = {
    nix.distributedBuilds = true;
  };
}

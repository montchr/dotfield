{
  imports = [ ./__lib.nix ];

  dotfield.modules.remote-builders.nixos = {
    nix.distributedBuilds = true;
  };
}

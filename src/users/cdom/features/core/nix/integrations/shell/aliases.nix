{ lib', ... }:
{
  dotfield.features.development.home = (
    lib'.shell.makeShellAliasesModule {
      abbrs = {
        n = "nix";
        nd = "nix develop";
        ndn = "nix develop nixpkgs#";
        ns = "nix shell";
        nsn = "nix shell nixpkgs#";
        nb = "nix build";
        nbn = "nix build nixpkgs#";
        nf = "nix flake";
      };
    }
  );
}

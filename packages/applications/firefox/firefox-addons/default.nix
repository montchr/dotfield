{
  callPackage,
  callPackages,
}: let
  buildFirefoxXpiAddon = callPackage ./buildFirefoxXpiAddon.nix {};
in
  callPackages ./addons.generated.nix {inherit buildFirefoxXpiAddon;}

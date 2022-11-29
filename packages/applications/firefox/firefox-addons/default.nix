{
  callPackage,
  callPackages,
}: let
  buildFirefoxXpiAddon = callPackage ./buildFirefoxXpiAddon.nix {};
in {
  inherit buildFirefoxXpiAddon;
  addons = callPackages ./addons.generated.nix {inherit buildFirefoxXpiAddon;};
}

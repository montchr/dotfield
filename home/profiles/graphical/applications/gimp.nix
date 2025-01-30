{ pkgs, ... }:
{
  home.packages = [
    # HACK: The `gap` plugin requires building from source due to continual
    # breakages and security issues, and I have no intent on using it anyway.
    # <https://github.com/NixOS/nixpkgs/pull/295257>
    # TODO: save as custom package elsewhere
    (pkgs.gimp-with-plugins.override {
      plugins =
        # <https://github.com/Scrumplex/nixpkgs/blob/cca25fd345f2c48de66ff0a950f4ec3f63e0420f/pkgs/applications/graphics/gimp/wrapper.nix#L5C1-L6C99>
        lib.filter (pkg: lib.isDerivation pkg && !pkg.meta.broken or false) (
          lib.attrValues (
            builtins.removeAttrs pkgs.gimpPlugins [
              "gimp"
              "gap"
              "gmic" # <https://github.com/NixOS/nixpkgs/pull/312997>
            ]
          )
        );
    })
  ];
}

{
  inputs = {
    root.url = "path:../../../";
    nixpkgs.follows = "root/nixpkgs";
    flake-parts.follows = "root/flake-parts";

    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ags = {
      url = "github:aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.astal.follows = "astal";
    };
  };

  outputs =
    inputs@{
      root,
      nixpkgs,
      flake-parts,
      ...
    }:
    (flake-parts.lib.mkFlake { inherit inputs; } (
      { ... }:
      {
        debug = true;
        systems = builtins.attrNames root.outputs.allSystems;
        perSystem =
          {
            inputs',
            config,
            pkgs,
            ...
          }:
          let
            pname = "dotfield-desktop-shell";
            astalPackages = with inputs'.ags.packages; [
              astal4
              io
              # notifd tray wireplumber ...
            ];
            extraPackages = astalPackages ++ [
              pkgs.libadwaita
              pkgs.libsoup_3
            ];
          in
          {
            devShells.default = pkgs.mkShell {
              buildInputs = [
                (inputs'.ags.packages.default.override {
                  inherit extraPackages;
                })
              ];
              nativeBuildInputs = [
                pkgs.biome
              ];
            };

            packages.default = pkgs.stdenv.mkDerivation {
              inherit pname;
              # TODO: use fileset
              src = ./src;

              nativeBuildInputs = [
                inputs'.ags.packages.default
                pkgs.gobject-introspection
                pkgs.wrapGAppsHook
              ];

              buildInputs = extraPackages ++ [ pkgs.gjs ];

              installPhase = ''
                runHook preInstall

                mkdir -p $out/{bin,share}
                cp -r * $out/share/
                ags bundle app.ts $out/bin/${pname} -d "SRC='$out/share'"

                runHook postInstall
              '';
            };
          };
      }
    ));
}

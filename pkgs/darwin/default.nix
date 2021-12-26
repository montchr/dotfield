{ self, inputs }:

final: prev: {
  prefmanager = inputs.prefmanager.defaultPackage.${prev.stdenv.system};
  yabai = final.callPackage (import ./yabai.nix) { };
}

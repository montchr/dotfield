{ self, inputs }:

final: prev: {
  yabai = final.callPackage (import ./yabai.nix) { };
}

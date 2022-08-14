final: prev: {
  trellis-cli = final.callPackage ./trellis-cli {inherit (final) sources;};
}

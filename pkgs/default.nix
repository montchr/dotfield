final: prev: {
  # keep sources this first
  sources = prev.callPackage (import ./_sources/generated.nix) { };
  # then, call packages with `final.callPackage`

  dotfield = prev.callPackage (import ./dotfield.nix) { };
} // (if prev.stdenv.isDarwin then {
  # Provide a fake `kitty` package because Homebrew handles this on darwin
  kitty = prev.runCommand "kitty-0.0.0" { } "mkdir $out";
} else { })

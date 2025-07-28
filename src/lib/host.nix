{ inputs, ... }:
let
  mkMetadata =
    name: attrs:
    let
      keys = inputs.haumea.lib.load {
        # FIXME: this should be relative to the invokee location... is
        # it though? i doubt it.
        src = ./keys;
        loader = [ (inputs.haumea.lib.matchers.always (_: builtins.readFile)) ];
      };
    in
    {
      age = keys."${name}.age";
      keys = [
        keys.${name}
        keys."${name}-rsa"
      ];
    }
    // attrs;
in
{
  flake.lib = { inherit mkMetadata; };
}

{ self, ... }:
let
  inherit (self.lib.modules) collectTypedModules;
in
{
  flake.tests.lib.modules = {
    testCollectTypedModules = {
      expr = collectTypedModules "foo" [
        {
          foo = {
            imports = [
              "one"
              "two"
            ];
          };
          bar = {
            imports = [
              "three"
              "four"
            ];
          };
        }
        {
          foo = {
            imports = [
              "five"
              "six"
            ];
          };
        }
      ];
      expected = [
        "one"
        "two"
        "five"
        "six"
      ];
    };
  };
}

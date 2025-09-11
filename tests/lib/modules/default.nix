{ self, ... }:
let
  inherit (self.lib.modules) collectTypedModules collectRequires;
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
        "five"
        "six"
        "one"
        "two"
      ];
    };

    testCollectRequires =
      let
        aspect1 = {
          name = "aspect1";
          nixos = {
            a = "b";
            b = "c";
          };
        };
        aspect2 = {
          name = "aspect2";
          requires = [ ];
          home = {
            a = "1";
            b = "2";
          };
        };
        aspect3 = {
          name = "aspect3";
          requires = [
            "aspect1"
            "aspect2"
          ];
          nixos = {
            c = "sea";
            d = "dee";
          };
        };
        aspect4 = {
          name = "aspect4";
          requires = [
            "aspect1"
            "aspect3"
          ];
        };
        aspects = {
          inherit
            aspect1
            aspect2
            aspect3
            aspect4
            ;
        };
      in
      {
        expr = collectRequires aspects [ aspects.aspect4 ];
        expected = [
          aspect1
          aspect2
          aspect3
        ];
      };
  };
}

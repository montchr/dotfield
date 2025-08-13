let
  inherit (inputs) haumea;

  hostName = "hodgepodge";
  keys = haumea.lib.load {
    src = ./keys;
    loader = [ (haumea.lib.matchers.always (_: builtins.readFile)) ];
  };
in

{

}

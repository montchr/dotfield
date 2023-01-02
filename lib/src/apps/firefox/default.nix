{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  mixins.monospaceText = import ./mixins/monospaceText.nix;
  paths = import ./paths.nix;

  toUserPrefLine = k: v: "user_pref(\"${k}\", ${l.toJSON v});\n";

  evalSettings = {
    modules,
    theme,
    osConfig ? null,
  }:
    l.evalModules {
      modules =
        modules
        ++ (l.singleton {
          _module.args = {inherit osConfig theme;};
          _module.freeformType = with l.types;
            lazyAttrsOf (oneOf [bool int str]);
        });
      specialArgs = {inherit inputs;};
    };
}

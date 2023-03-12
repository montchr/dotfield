{
  l,
  withSystem,
  ...
}: {
  mixins.monospaceText = import ./mixins/monospaceText.nix;
  paths = import ./paths.nix;

  toUserPrefLine = k: v: "user_pref(\"${k}\", ${l.toJSON v});\n";

  evalSettings = {
    system,
    modules,
    theme,
    hmConfig ? null,
    osConfig ? null,
  }:
    withSystem system (
      {inputs', ...}:
        l.evalModules {
          modules =
            modules
            ++ (l.singleton {
              _module.args = {inherit hmConfig osConfig theme;};
              _module.freeformType = with l.types;
                lazyAttrsOf (oneOf [bool int str]);
            });
        }
    );
}

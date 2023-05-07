{l, ...}: {
  colors,
  fonts,
}: let
  inherit (builtins) attrValues mapAttrs;
  mkVar = type: name: value: "--dotfield--${type}--${name}: ${value};";
  mkVarDecls = type: attrs:
    l.concatLines (attrValues
      (mapAttrs (mkVar type) attrs));
in ''
  :root {
    ${mkVarDecls "color" colors}

    ${mkVar "font" "mono-family" ''"${fonts.mono.family}"''}
    ${mkVar "font" "sans-family" ''"${fonts.sans.family}"''}
    ${mkVar "font" "serif-family" ''"${fonts.serif.family}"''}
  }
''

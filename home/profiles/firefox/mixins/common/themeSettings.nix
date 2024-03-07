{ l, ... }:
{
  # colors,
  fonts,
}:
let
  # inherit (builtins) attrValues mapAttrs;
  # mkVar = type: name: value: "--dotfield--${type}--${name}: ${value};";
  # mkVarDecls = type: attrs:
  #   l.concatLines (attrValues
  #     (mapAttrs (mkVar type) attrs));
  # TODO: move to apparat
  quote = s: ''"${l.toString s}"'';
  fontStack = l.concatMapStringsSep ", " quote;
  fontStack' = fallback: x: (fontStack (l.map (y: y.name) (l.toList x))) + ", ${fallback}";
in
with fonts;
''
  --dotfield--font--mono: ${fontStack' "monospace" monospace};
  --dotfield--font--sans: ${fontStack' "sans-serif" sansSerif};
  --dotfield--font--serif: ${fontStack' "serif" serif};

  --dotfield--font--mono-size: 12;
  --dotfield--font--sans-size: 12;
  --dotfield--font--serif-size: 12;
''

{l, ...}: {
  colors,
  fonts,
}: let
  inherit (builtins) attrValues mapAttrs;

  mkVar = type: name: value: "--dotfield--${type}--${name}: ${value};";
  mkVarDecls = type: attrs:
    l.concatLines (attrValues
      (mapAttrs (mkVar type) attrs));

  # TODO: move to apparat
  quote = s: ''"${l.toString s}"'';
  fontStack = l.concatMapStringsSep ", " quote;
  fontStack' = fallback: x:
    (fontStack (l.map (y: y.family) (l.toList x)))
    + ", ${fallback}";
in
  with fonts; ''
    --dotfield--font--mono: ${fontStack' "monospace" mono};
    --dotfield--font--sans: ${fontStack' "sans-serif" sans};
    --dotfield--font--serif: ${fontStack' "serif" serif};
    --dotfield--font--term: ${fontStack' "monospace" [term mono]};

    --dotfield--font--mono-size: ${l.toString mono.size};
    --dotfield--font--sans-size: ${l.toString sans.size};
    --dotfield--font--serif-size: ${l.toString serif.size};
    --dotfield--font--term-size: ${l.toString term.size};

    ${mkVarDecls "color" colors}
  ''

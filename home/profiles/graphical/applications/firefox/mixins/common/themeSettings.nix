{ lib, ... }:
{ fonts }:
let
  # TODO: move to apparat
  quote = s: ''"${builtins.toString s}"'';
  fontStack = lib.concatMapStringsSep ", " quote;

  # TODO: this seems overly-complicated -- combine with `fontStack`
  fontStack' = fallback: x: (fontStack (builtins.map (y: y.name) (lib.toList x))) + ", ${fallback}";
in
''
  --dotfield--font--mono: ${fontStack' "monospace" fonts.monospace};
  --dotfield--font--sans: ${fontStack' "sans-serif" fonts.sansSerif};
  --dotfield--font--serif: ${fontStack' "serif" fonts.serif};

  --dotfield--font--mono-size: 12;
  --dotfield--font--sans-size: 12;
  --dotfield--font--serif-size: 12;
''

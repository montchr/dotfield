{ ... }:
let
  engine = template: { urls = [ { inherit template; } ]; };
  withAlias = s: attrs: attrs // { definedAliases = [ s ]; };
in
{
  inherit engine;

  engine' = alias: template: withAlias "@${alias}" (engine template);
}

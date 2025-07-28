let
  engine = template: { urls = [ { inherit template; } ]; };
  engine' = alias: template: withAlias "@${alias}" (engine template);
  withAlias = s: attrs: attrs // { definedAliases = [ s ]; };
in
{
  flake.lib.firefox = { inherit engine engine'; };
}

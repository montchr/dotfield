{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;

  makeLuaPlugin = attrs:
    attrs
    // {
      type = "lua";
      config =
        l.optionalString
        (attrs ? config && attrs.config != null)
        (makeLuaBlock attrs.plugin.pname attrs.config);
    };

  makeLuaBlock = title: content: ''
    --: ${title} {{{
    ${content}
    --: }}}
  '';
in {
  inherit makeLuaBlock makeLuaPlugin;

  makeLuaPlugin' = plugin: config: makeLuaPlugin {inherit plugin config;};
}

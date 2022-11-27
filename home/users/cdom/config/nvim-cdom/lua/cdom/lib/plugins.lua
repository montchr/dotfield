local _2afile_2a = "/Users/cdom/.config/nvim/fnl/cdom/lib/plugins.fnl"
local _2amodule_name_2a = "cdom.lib.plugins"
local _2amodule_2a
do
  package.loaded[_2amodule_name_2a] = {}
  _2amodule_2a = package.loaded[_2amodule_name_2a]
end
local _2amodule_locals_2a
do
  _2amodule_2a["aniseed/locals"] = {}
  _2amodule_locals_2a = (_2amodule_2a)["aniseed/locals"]
end
local autoload = (require("aniseed.autoload")).autoload
local a = autoload("aniseed.core")
do end (_2amodule_locals_2a)["a"] = a
local function safe_require_plugin_config(name)
  local ok_3f, val_or_err = pcall(require, ("cdom.plugin." .. name))
  if not ok_3f then
    return print(("Plugin config error: " .. val_or_err))
  else
    return nil
  end
end
_2amodule_locals_2a["safe-require-plugin-config"] = safe_require_plugin_config
local function req(name)
  return ("require('cdom.plugin." .. name .. "')")
end
_2amodule_2a["req"] = req
return _2amodule_2a
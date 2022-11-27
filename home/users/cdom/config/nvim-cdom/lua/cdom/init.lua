local _2afile_2a = "/Users/cdom/.config/nvim/fnl/cdom/init.fnl"
local _2amodule_name_2a = "cdom.init"
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
local l, nvim = autoload("cdom.lib"), autoload("aniseed.nvim")
do end (_2amodule_locals_2a)["l"] = l
_2amodule_locals_2a["nvim"] = nvim
nvim.o.termguicolors = true
nvim.o.mouse = "a"
nvim.o.updatetime = 500
nvim.o.timeoutlen = 500
nvim.o.sessionoptions = "blank,curdir,folds,help,tabpages,winsize"
nvim.o.inccommand = "split"
nvim.ex.set("list")
nvim.g.mapleader = " "
nvim.g.maplocalleader = ","
return _2amodule_2a
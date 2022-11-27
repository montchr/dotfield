local execute = vim.api.nvim_command
local fn = vim.fn
local fmt = string.format

local packPath = fn.stdpath("data") .. "/site/pack"

function ensure (user, repo)
  local installPath = fmt("%s/packer/start/%s", packPath, repo, repo)
  if fn.empty(fn.glob(installPath)) > 0 then
    execute(fmt("!git clone https://github.com/%s/%s %s", user, repo, installPath))
    execute(fmt("packadd %s", repo))
  end
end

ensure("wbthomason", "packer.nvim")

ensure("Olical", "aniseed")
vim.g["aniseed#env"] = {module = "mtgoch.init"}


-- require "user.impatient"
-- require "user.options"
-- require "user.keymaps"


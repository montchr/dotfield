-- Load the Hammerspoon CLI tools.
local ipc = require("hs.ipc")
if not ipc.cliStatus("/usr/local") then
  ipc.cliInstall()
end

-- Stackline: yabai stack visualization
-- https://github.com/AdamWagner/stackline
stackline = require "stackline.stackline.stackline"

local stacklineConfig = {
  appearance = {
    showIcons = false,
  }
}

stackline:init(stacklineConfig)

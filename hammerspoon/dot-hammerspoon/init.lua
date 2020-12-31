-- Stackline: yabai stack visualization
-- https://github.com/AdamWagner/stackline
stackline = require "stackline.stackline.stackline"

local stacklineConfig = {
  appearance = {
    showIcons = false,
  }
}

stackline:init(stacklineConfig)

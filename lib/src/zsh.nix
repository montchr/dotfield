# <https://github.com/NixOS/nixpkgs/blob/3620d64912f35e984fb5a1ce61b6c29fb48072d8/lib/modules.nix#L1017-L1>
{ lib, ... }:
let
  profiler = 601;
  # beforeInstantPrompt = instantPrompt - 1;
  instantPrompt = 610;
  # afterInstantPrompt = instantPrompt + 1;
  rcPreset = 620;
  pluginManager = 630;
  user = 640;
  # prompt = 690;

  mkInitProfiler = lib.mkOrder profiler;
  # mkBeforeInstantPrompt = lib.mkOrder beforeInstantPrompt;
  mkInitInstantPrompt = lib.mkOrder instantPrompt;
  # mkAfterInstantPrompt = lib.mkOrder afterInstantPrompt;
  mkInitConfigPreset = lib.mkOrder rcPreset;
  mkInitPluginManager = lib.mkOrder pluginManager;
  # mkInitPrompt = lib.mkOrder prompt;
  mkInitUserConfig = lib.mkOrder user;
in
{
  inherit
    mkInitProfiler
    mkInitInstantPrompt
    # mkBeforeInstantPrompt

    # mkAfterInstantPrompt

    mkInitConfigPreset
    mkInitPluginManager
    mkInitUserConfig
    ;
}

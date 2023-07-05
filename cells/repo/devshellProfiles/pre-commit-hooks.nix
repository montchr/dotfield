# FIXME: unusable in its current state -- probably better off without the cachix flake
{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;
  pre-commit = inputs.pre-commit-hooks.lib.run {
    src = ../..;
    hooks = {
      alejandra.enable = true;
      deadnix.enable = true;
      markdownlint.enable = true;
      nil.enable = true;
      # prettier.enable = true;
      shellcheck.enable = true;
      # shfmt.enable = true;
      statix.enable = true;
      # TODO: needs wrapped package: <https://github.com/cachix/pre-commit-hooks.nix/blob/8d316204b4b977202551d98ab51a7b8c9898afca/modules/hooks.nix#L452C1-L463C18>
      # treefmt.enable = true;
    };
    default_stages = ["commit" "push"];
  };
in {
  packages = [pre-commit];
  devshell.startup.pre-commit-hooks = l.stringsWithDeps.noDepEntry ''
    ${pre-commit.shellHook}
  '';
}

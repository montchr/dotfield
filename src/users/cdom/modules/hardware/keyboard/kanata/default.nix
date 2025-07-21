{
  flake.modules.nixos.workstation = {
    services.kanata.keyboards."default" = {
      config = ''
        ;; Machine-specific configurations can be appended to this initial
        ;; hardware-specific configuration, provided that each configuration share
        ;; the same `defsrc` block.

        ${builtins.readFile ./default.kbd}
      '';
    };
  };
}

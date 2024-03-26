##: MacBook14,2 (A2681) -- <https://everymac.com/systems/apple/macbook-air/specs/macbook-air-m2-8-core-cpu-8-core-gpu-13-2022-specs.html>
{
  config,
  # lib,
  # pkgs,
  ...
}:
let
  inherit (builtins) readFile;
  kbdComment = ''
    ;; Machine-specific configurations can be appended to this initial
    ;; hardware-specific configuration, provided that each configuration share
    ;; the same `defsrc` block.
  '';
in
{
  imports = [
    ./apple-silicon.nix
    ./macbook.nix
  ];

  services.keyd.keyboards."apple-mtp-keyboard" = {
    # NOTE: The `k` is important here, because the touchpad has the same
    # hardware ID! See `keyd (1)` for more info.
    ids = [ "k:0fac:0ade" ];
    settings.main = { };
  };

  services.kanata.keyboards = {
    "default".config = ''
      ${kbdComment}
      ${readFile ./kanata-default.kbd}
    '';

    # NOTE: should not be enabled at the same time as Default! there will be a
    # warning explaining that a race condition may result.
    # "apple-mtp-keyboard".config = ''
    #   ${kbdComment}
    #   ${readFile ./kanata-default.kbd}
    # '';
  };

  services.kmonad.keyboards."default".config = ''
    ${kbdComment}
    ${builtins.readFile ./kmonad-default.kbd}
  '';
}

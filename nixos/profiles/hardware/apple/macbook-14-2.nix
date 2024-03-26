##: MacBook14,2 (A2681) -- <https://everymac.com/systems/apple/macbook-air/specs/macbook-air-m2-8-core-cpu-8-core-gpu-13-2022-specs.html>
let
  inherit (builtins) readFile;
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

  services.kmonad.keyboards."default".config = ''
    ;; Machine-specific configurations can be appended to this initial
    ;; hardware-specific configuration.
    ${builtins.readFile ./kmonad-default.kbd}
  '';
}

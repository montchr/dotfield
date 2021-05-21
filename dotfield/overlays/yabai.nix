final: prev: {
  # https://github.com/NixOS/nixpkgs/pull/108861#issuecomment-832087889
  yabai = prev.yabai.overrideAttrs (
    let
      version = "3.3.8";
    in
      o: {
        src = builtins.fetchTarball {
          url =
            "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
          sha256 = "1qh1vf52j0b3lyrm005c8c98s39rk1lq61rrq0ml2yr4h77rq3xv";
        };

        installPhase = ''
          mkdir -p $out/bin
          mkdir -p $out/share/man/man1/
          cp ./bin/yabai $out/bin/yabai
          cp ./doc/yabai.1 $out/share/man/man1/yabai.1
        '';
      }
  );
}

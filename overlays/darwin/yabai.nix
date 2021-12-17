final: prev: {
  yabai = prev.yabai.overrideAttrs (o: rec {
    version = "4.0.0-pre";

    src = final.fetchFromGitHub {
      owner = "koekeishiya";
      repo = "yabai";
      # rev = "v${version}";
      rev = "f403e609e32b4100494c5afb089d0010e7e4ef91";
      sha256 = "sha256-Lzim9h9aZopS7BjLzGghZQpgHx183psTLHCM9ndJCXo=";
    };

    # XCODE_APP = final.darwin.xcode;

    buildInputs = with final.darwin.apple_sdk.frameworks; [
      Carbon
      Cocoa
      CoreServices
      ScriptingBridge
      SkyLight
    ];

    stdenv = final.clangStdenv;

    nativeBuildInputs = with final; [
      # clang_13
      xcbuild
      darwin.xcode
      xxd
    ];

    # dontBuild = true;
    buildPhase = "";

    preInstall = ''
      make clean
    '';

    installPhase = "make install";

    postInstall = ''
      mkdir -p $out/bin
      mkdir -p $out/share/man/man1/
      cp ./bin/yabai $out/bin/yabai
      cp ./doc/yabai.1 $out/share/man/man1/yabai.1
    '';
  });
}

final: prev: {
  pure-prompt = prev.pure-prompt.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./pure-zsh.patch ];
  });

  igloo-prompt = with prev.lib; prev.stdenv.mkDerivation rec {
    name = "igloo-prompt";
    version = "0.1.1";

    src = prev.fetchFromGitHub {
      owner = "arcticicestudio";
      repo = "igloo";
      rev = "eaa4f60ce760f58337bfdc28b920c78498fba1ab";
      sha256 = "2CPzDnXfEuoOnvEZiC3I5kAsWYvxI9n0VtZElHf3LYs=";
    };

    installPhase = ''
      OUTDIR="$out/share/zsh/site-functions"
      mkdir -p "$OUTDIR"
      cp snowblocks/zsh/lib/themes/igloo.zsh "$OUTDIR/prompt_igloo_setup"
    '';

    meta = {
      description = "Minimal and uncluttered ZSH prompt";
      homepage = "https://github.com/arcticicestudio/igloo/tree/master/snowblocks/zsh";
      licence = licences.mit;
      platforms = platforms.all;
      maintainers = with maintainers; [ ];
    };
  };
}

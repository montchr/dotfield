{
  lib,
  fetchFromGitHub,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "fre";
  version = "unstable-2022-08-10";

  src = fetchFromGitHub {
    owner = "camdencheek";
    repo = "fre";
    rev = "4b8b0209a19647f91edb2e9c2b36488c915a0711";
    sha256 = "sha256-gXUz0hfAbbIpLNhnxaWUfJqEbxuOUUZVgXind7Tsmsk=";
  };

  cargoSha256 = "sha256-niTPJ3A5hp0uyzmlWKN2sckn29dwYhGMUBVgbJS3yj4=";

  meta = {
    description = "Command line frecency tracking";
    longDescription = ''
      `fre` is a CLI tool for tracking your most-used directories and files.

      Though inspired by tools like `autojump` or the `z` plugin for `zsh`,
      `fre` does not support jumping. It is primarily designed to interface with `fzf`.

      Additionally, `fre` uses an algorithm in which the weights of each directory decay exponentially,
      so more recently used directories are ranked more highly in a smooth manner.
    '';
    homepage = "https://github.com/camdencheek/fre";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [montchr];
    platforms = lib.platforms.unix;
  };
}

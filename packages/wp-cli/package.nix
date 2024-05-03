{
  lib,
  stdenv,
  fetchFromGitHub,
  php,
}:

php.buildComposerProject (finalAttrs: {
  pname = "wp-cli";
  version = "2.10.0";

  src = fetchFromGitHub {
    owner = "wp-cli";
    repo = "wp-cli";
    rev = "v${finalAttrs.version}";
    hash = "sha256-YjwTGWyZ9wD7wDXWTMaEuK/fo7sFYeMV+QcwQvl/5hc=";
  };

  meta = with lib; {
    description = "WP-CLI framework";
    homepage = "https://github.com/wp-cli/wp-cli/";
    license = licenses.mit;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "wp";
    platforms = platforms.all;
  };
})

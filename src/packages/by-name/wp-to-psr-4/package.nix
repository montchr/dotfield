{
  lib,
  php82,
  fetchFromGitHub,
}:

php82.buildComposerProject (finalAttrs: {
  pname = "wp-to-psr-4";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "alleyinteractive";
    repo = "wp-to-psr-4";
    rev = "v${finalAttrs.version}";
    hash = "sha256-JYl3Z/hNcCcHnLuKXVkAtMeihXM0NZbFZfen0Hl1gos=";
  };

  vendorHash = "sha256-1RqtUKK/H7Uc0phe/c7miviNirds23BvPaKaS1hTVog=";

  composerLock = ./composer.lock;

  meta = with lib; {
    description = "Migrate a WordPress Codebase to PSR-4";
    homepage = "https://github.com/alleyinteractive/wp-to-psr-4";
    changelog = "https://github.com/alleyinteractive/wp-to-psr-4/blob/${src.rev}/CHANGELOG.md";
    license = licenses.gpl2Only;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "wp-to-psr-4";
    platforms = platforms.all;
  };
})

{
  lib,
  php,
  fetchFromGitHub,
}:

php.buildComposerProject (finalAttrs: {
  pname = "php-stubs-generator";
  version = "0.8.4";

  src = fetchFromGitHub {
    owner = "php-stubs";
    repo = "generator";
    rev = "v${finalAttrs.version}";
    hash = "sha256-rJRl9SOldVD7OrDFVZkbHlTcqLok6n2zTH6wdJeoU+M=";
  };

  vendorHash = "sha256-N12FkmKtIw/IXsMXUcf1ql4TlLg/OScmfKgvJpuHF+Q=";

  composerLock = ./composer.lock;

  meta = with lib; {
    description = "Generate stubs from any PHP code for IDE completion and static analysis";
    homepage = "https://github.com/php-stubs/generator";
    changelog = "https://github.com/php-stubs/generator/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "generate-stubs";
    platforms = platforms.all;
  };
})

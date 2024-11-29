{
  lib,
  buildGoModule,
  fetchFromSourcehut,
}:

buildGoModule rec {
  pname = "scotty";
  version = "0.4.1";

  src = fetchFromSourcehut {
    owner = "~phw";
    repo = "scotty";
    rev = "v${version}";
    hash = "sha256-R9MgX5Am//9ULNKWqfH/JthMhqg4FFitOds7wU5iDSU=";
  };

  vendorHash = "sha256-wlyoN6y/Bh8f21d5bIzwRwWkGlXbFFx+uGK6c5PyKTs=";

  ldflags = [
    "-X main.Version=${version}"
    "-X main.Commit=${version}"

    "-s" # disable symbol table (from nix-init)
    "-w" # disable DWARF generation (from nix-init)
  ];

  # Prevent test failure due to locale detection with Xuanwo/go-locale.
  preCheck = ''
    export LANG="en_US.UTF-8"
    export LOCALE_ARCHIVE=$glibcLocales/lib/locale/locale-archive
  '';

  meta = {
    description = "";
    homepage = "https://git.sr.ht/~phw/scotty";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "scotty";
  };
}

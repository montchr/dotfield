{
  lib,
  rustPlatform,
  fetchFromGitHub,
  installShellFiles,
  testers,
  fd,
}:
rustPlatform.buildRustPackage rec {
  pname = "fd";
  version = "8.7.0-patched";

  src = fetchFromGitHub {
    owner = "sharkdp";
    repo = "fd";
    rev = "v8.7.0";
    hash = "sha256-y7IrwMLQnvz1PeKt8BE9hbEBwQBiUXM4geYbiTjMymw=";
  };

  cargoHash = "sha256-NrMSspl5IRiiUFH7iicJfcLzIRmslTdbRRL7otSUudE=";

  nativeBuildInputs = [installShellFiles];

  # skip flaky test
  checkFlags = [
    "--skip=test_owner_current_group"
  ];

  # Asahi Linux 16k page compatibility.
  # <https://github.com/sharkdp/fd/issues/1085>
  # <https://github.com/NixOS/nixpkgs/issues/202863>
  JEMALLOC_SYS_WITH_LG_PAGE = 16;

  postInstall = ''
    installManPage doc/fd.1

    installShellCompletion --cmd fd \
      --bash <($out/bin/fd --gen-completions bash) \
      --fish <($out/bin/fd --gen-completions fish)
    installShellCompletion --zsh contrib/completion/_fd
  '';

  passthru.tests.version = testers.testVersion {
    package = fd;
  };

  meta = with lib; {
    description = "A simple, fast and user-friendly alternative to find";
    longDescription = ''
      `fd` is a simple, fast and user-friendly alternative to `find`.

      While it does not seek to mirror all of `find`'s powerful functionality,
      it provides sensible (opinionated) defaults for 80% of the use cases.
    '';
    homepage = "https://github.com/sharkdp/fd";
    changelog = "https://github.com/sharkdp/fd/blob/v${version}/CHANGELOG.md";
    license = with licenses; [
      asl20
      /*
      or
      */
      mit
    ];
    maintainers = with maintainers; [dywedir figsoda globin ma27 zowoq];
  };
}

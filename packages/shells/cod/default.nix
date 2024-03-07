{
  stdenv,
  lib,
  fetchFromGitHub,
  buildGoModule,
  python3,
}:
buildGoModule rec {
  pname = "cod";
  version = "unstable-2023-05-26";

  src = fetchFromGitHub {
    owner = "dim-an";
    repo = "cod";
    rev = "fdbe85afb8790821de1e48a3c8645ca0118ab4b9";
    hash = "sha256-OlVYHOOSnwhoiy7bnhROGA3SI+6jaK6JZDzekRKOm3c=";
  };

  vendorHash = "sha256-bJdyRFW8bPoweI2V5n/WBUEqlQ33QT4FqLTQtiKHYic=";

  ldflags = [
    "-s"
    "-w"
    "-X main.GitSha=${src.rev}"
  ];

  nativeCheckInputs = [ python3 ];

  preCheck = ''
    pushd test/binaries/
    for f in *.py; do
      patchShebangs ''$f
    done
    popd
    export COD_TEST_BINARY="''${NIX_BUILD_TOP}/go/bin/cod"

    substituteInPlace test/learn_test.go --replace TestLearnArgparseSubCommand SkipLearnArgparseSubCommand
  '';

  meta = with lib; {
    description = "Tool for generating Bash/Fish/Zsh autocompletions based on `--help` output";
    homepage = "https://github.com/dim-an/cod/";
    license = licenses.asl20;
    maintainers = with maintainers; [
      SuperSandro2000
      montchr
    ];
  };
}

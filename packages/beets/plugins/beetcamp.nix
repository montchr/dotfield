{
  lib,
  beets,
  python3,
  fetchFromGitHub,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "beetcamp";
  version = "0.20.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "snejus";
    repo = "beetcamp";
    rev = version;
    hash = "sha256-k8IbzD59PU7iSUUe4USu45fFyob8mWe0EGreWt2x6xI=";
  };

  build-system = [
    python3.pkgs.poetry-core
  ];

  nativeBuildInputs = [
    beets
  ];

  dependencies = with python3.pkgs; [
    httpx
    ordered-set
    packaging
    pycountry
    requests
  ];

  preBuild = ''
    export HOME=$(mktemp -d)
  '';

  nativeCheckInputs = with python3.pkgs; [
    # FIXME: needs rich-tables
    # pytestCheckHook
    beets

    # test dependencies:

    # gitpython
    # rich
    # TODO: package this
    # rich-tables
  ];

  pythonImportsCheck = [
    "beetsplug.bandcamp"
  ];

  meta = {
    description = "Bandcamp autotagger source for beets";
    homepage = "https://github.com/snejus/beetcamp";
    changelog = "https://github.com/snejus/beetcamp/blob/${src.rev}/CHANGELOG.md";
    license = lib.licenses.gpl2Only;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "beetcamp";
  };
}

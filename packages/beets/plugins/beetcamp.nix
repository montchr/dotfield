{
  lib,
  python3,
  fetchFromGitHub,

  beets,
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

  # FIXME: tests require a git repo!  but this probably could be avoided
  # upstream by making the use of `gitpython` optional.  this would likely
  # require a refactor, as i don't think it is possible to skip
  # `tests/conftest.py`.  the `gitpython` package disables checks because they
  # require a git repo:
  # <https://github.com/NixOS/nixpkgs/blob/e8c38b73aeb218e27163376a2d617e61a2ad9b59/pkgs/development/python-modules/gitpython/default.nix#L38-L39>
  doCheck = false;

  nativeCheckInputs = (
    #  with python3.pkgs;
    [
      beets
      # rich-tables
      # pytestCheckHook
      # gitpython
      # rich
    ]);

  # pytestFlagsArray = [ "-r fEs" ];

  # disabledTests = [
  #   # <https://github.com/snejus/beetcamp/blob/b356bf6379ff887d78a281e88f5fc32f93287e3e/.github/workflows/build.yml#L31>
  #   "need_connection"
  # ];

  # disabledTestPaths = [
  #   "tests/conftest.py"

  #   # <https://github.com/snejus/beetcamp/blob/b356bf6379ff887d78a281e88f5fc32f93287e3e/.github/workflows/build.yml#L31>
  #   "tests/test_lib.py"
  # ];

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

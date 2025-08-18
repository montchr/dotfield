{
  lib,
  python3,
  fetchFromGitHub,
  beets,
}:

python3.pkgs.buildPythonApplication {
  pname = "beet-summarize";
  version = "2024.11.04";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "steven-murray";
    repo = "beet-summarize";
    rev = "f2f3d20f3933480f6611c88dc3f5b043f83cfe75";
    hash = "sha256-WDEfMbL5jzMVgGejXwIi5UjkrgN2hw/d8EL+B5aSycI=";
  };

  build-system = [
    python3.pkgs.setuptools
    python3.pkgs.setuptools-scm
  ];

  preBuild = ''
    export HOME=$(mktemp -d)
  '';

  nativeCheckInputs = [
    python3.pkgs.pytestCheckHook
    beets
  ];

  pythonImportsCheck = [
    "beetsplug.summarize"
  ];

  meta = {
    description = "Summarize beets library statistics";
    homepage = "https://github.com/steven-murray/beet-summarize";
    license = lib.licenses.lgpl3;
    maintainers = with lib.maintainers; [ montchr ];
    inherit (beets.meta) platforms;
  };
}

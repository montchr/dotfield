{
  lib,
  python3,
  fetchFromGitHub,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "rgbxy";
  version = "unstable-2021-05-11";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "benknight";
    repo = "hue-python-rgb-converter";
    rev = "f73a4ecb5dd0c5050edbfc460a696da685d441d7";
    hash = "sha256-5ApmwQpIy73Iu14+Pp0JeLvaCd6Lj8XEvrolIoxUP0s=";
  };

  build-system = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  pythonImportsCheck = [
    "rgbxy"
  ];

  meta = {
    description = "RGB conversion tool written in Python for Philips Hue";
    homepage = "https://github.com/benknight/hue-python-rgb-converter";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "rgbxy";
  };
}

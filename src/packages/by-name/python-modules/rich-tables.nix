{
  lib,
  python3,
  fetchFromGitHub,

  rgbxy,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "rich-tables";
  version = "0.6.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "snejus";
    repo = "rich-tables";
    rev = version;
    hash = "sha256-4TJni2p0X03TI8RrFpV+U8QZ4IJYb/eIZv4k16g2GfM=";
  };

  build-system = [
    python3.pkgs.poetry-core
  ];

  dependencies = (
    with python3.pkgs;
    [
      funcy
      multimethod
      platformdirs
      rich
      sqlparse
      typing-extensions
    ]
  );

  optional-dependencies = {
    hue = [
      rgbxy
    ];
  };

  pythonImportsCheck = [
    "rich_tables"
  ];

  meta = {
    description = "Some ready-made rich tables for JSON data";
    homepage = "https://github.com/snejus/rich-tables";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "rich-tables";
  };
}

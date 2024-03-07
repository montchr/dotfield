{
  lib,
  python3,
  fetchFromGitHub,
}:
python3.pkgs.buildPythonApplication rec {
  pname = "synadm";
  version = "0.42";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "JOJ0";
    repo = "synadm";
    rev = "v${version}";
    hash = "sha256-vUYKylcfJ9YGZ6izv9Kdz2K2EhSZ0CxmXg8fVNw45/M=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    click
    click-option-group
    dnspython
    pyyaml
    requests
    tabulate
  ];

  pythonImportsCheck = [ "synadm" ];

  meta = with lib; {
    description = "Command line admin tool for Synapse (the Matrix reference homeserver";
    homepage = "https://github.com/JOJ0/synadm";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ montchr ];
  };
}

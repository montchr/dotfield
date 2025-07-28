# TODO: how can this read be made a default based on the user
# directory?  add all files with ".pub" extension (except, somehow,
# the ".age.pub" files...).
[
  ./blink-at-brakhage.pub
  ./cdom-at-ryosuke.pub
  ./cdom-at-tuuvok.pub
  ./cdom-at-tuvix.pub
  ./cdom-yubikey-rsa.pub
  ./seadoom-at-boschic.pub
  ./seadoom-at-hodgepodge.pub
]
|> builtins.map builtins.readFile

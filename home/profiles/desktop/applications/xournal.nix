# Xournal++ is an open-source and cross-platform note-taking software that is
# fast, flexible, and functional. A modern rewrite and a more feature-rich
# version of the wonderful Xournal program.
#
# <https://xournalpp.github.io/>
#
# It also functions as a PDF annotator:
# <https://xournalpp.github.io/guide/pdfs/>
{ pkgs, ... }:
{
  home.packages = [ pkgs.xournalpp ];
}

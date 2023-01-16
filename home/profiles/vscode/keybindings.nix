[
  {
    command = "cursorTop";
    key = "g g";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "magit.refresh";
    key = "g r";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "extension.vim_tab";
    key = "tab";
    when = "editorFocus && vim.active && !inDebugRepl && vim.mode != 'Insert' && editorLangId != 'magit'";
  }
  {
    command = "-extension.vim_tab";
    key = "tab";
    when = "editorFocus && vim.active && !inDebugRepl && vim.mode != 'Insert'";
  }
  {
    command = "magit.discard-at-point";
    key = "x";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "-magit.discard-at-point";
    key = "k";
  }
  {
    command = "magit.reverse-at-point";
    key = "-";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "-magit.reverse-at-point";
    key = "v";
  }
  {
    command = "magit.reverting";
    key = "shift+-";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "-magit.reverting";
    key = "shift+v";
  }
  {
    command = "magit.resetting";
    key = "shift+o";
    when = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
  }
  {
    command = "-magit.resetting";
    key = "shift+x";
  }
  {
    command = "-magit.reset-mixed";
    key = "x";
  }
  {
    command = "-magit.reset-hard";
    key = "ctrl+u x";
  }
  {
    command = "editor.action.marker.next";
    key = "] e";
    when = "editorFocus && vim.mode == 'Normal'";
  }
  {
    command = "-editor.action.marker.next";
    key = "alt+f8";
    when = "editorFocus && vim.mode == 'Normal'";
  }
  {
    command = "editor.action.marker.prev";
    key = "[ e";
    when = "editorFocus && vim.mode == 'Normal'";
  }
  {
    command = "-editor.action.marker.prev";
    key = "shift+alt+f8";
    when = "editorFocus && vim.mode == 'Normal'";
  }
]

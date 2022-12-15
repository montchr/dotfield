###: semantic color aliases
#
# - https://github.com/tinted-theming/base24/blob/master/styling.md
colors: (with colors; {
  background = base00;
  foreground = base05;
  selection.bg = base05;
  selection.fg = base00;

  syntax = {
    boolean = base09;
    class = base0A;
    comments = base03;
    constant = base09;
    delimiter = base05;
    escChars = base0C;
    function = base0D;
    inherited = base0B;
    integer = base09;
    invisibles = base03;
    keyword = base0E;
    operator = base05;
    regexp = base0C;
    string = base0B;
    variable = base08;
  };

  diff = {
    inserted = base0B;
    deleted = base08;
    changed = base0E;
  };

  element = {
    tag = base08;
    attr = base09;
    ids = base0D;
    selector = base0E;
  };

  markup = {
    heading = base0D;
    bold = base0A;
    italic = base0E;
    link.text = base08;
    link.url = base09;
    list = base08;
    code = base0B;
    quote = base0C;
  };

  ui = {
    border = {
      active = base03;
      inactive = base01;
    };
    cursor = base05;
    lineHighlight = base03;
    searchText.bg = base0A;
    statusBar = {
      bg = base01;
      fg = base04;
    };
    tab = {
      bar.bg = base01;
      active.bg = base00;
      active.fg = base05;
      inactive.bg = base01;
      inactive.fg = base04;
    };
  };
})

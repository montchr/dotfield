{
  root = true;

  "*" = {
    charset = "utf-8";
    end_of_line = "lf";
    indent_size = 2;
    indent_style = "space";
    insert_final_newline = true;
    trim_trailing_whitespace = true;
  };

  "*.{bash,sh}" = {
    binary_next_line = true;
    simplify = true;
    switch_case_indent = true;
  };

  "*.md" = {
    indent_style = "space";
    trim_trailing_whitespace = false;
  };

  "*.{diff,patch}" = {
    end_of_line = "unset";
    indent_size = "unset";
    insert_final_newline = "unset";
    trim_trailing_whitespace = "unset";
  };

  "*.{plist,php,py,xml}" = {
    indent_size = 4;
    indent_style = "space";
  };

  "{LICENSES/**,LICENSE,secrets/**}" = {
    charset = "unset";
    end_of_line = "unset";
    insert_final_newline = "unset";
    indent_size = "unset";
    indent_style = "unset";
    trim_trailing_whitespace = "unset";
  };
}

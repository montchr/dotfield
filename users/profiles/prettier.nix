{ config, lib, pkgs, ... }:

{
  my.hm.xdg.configFile."prettier/prettierrc.template".text = ''
    arrowParens: 'always'
    bracketSpacing: true
    jsxBracketSameLine: false
    jsxSingleQuote: false
    printWidth: 80
    proseWrap: 'never'
    quoteProps: 'as-needed'
    semi: true
    singleQuote: true
    tabWidth: 2
    trailingComma: 'all'
    useTabs: false
    overrides:
      - files: '*.php'
        options:
          braceStyle: '1tbs'
          phpVersion: '7.4'
          tabWidth: 4
          trailingCommaPHP: true
          useTabs: true
  '';
}

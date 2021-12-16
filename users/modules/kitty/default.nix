{ pkgs, lib, config, options, inputs, ... }:

{
  options = {
    # colors = {
    #   enable = mkOption {
    #     type = types.bool;
    #     default = false;
    #     description = ''
    #       When enabled, commands <command>term-dark</command> and <command>term-light</command> will
    #       toggle between dark and light colorschemes.
    #       <command>term-background</command> which accepts one argument (the value of which should
    #       be <literal>dark</literal> or <literal>light</literal>) is also avaible.
    #       (Note that the Kitty setting <literal>allow_remote_control = true</literal> is set to
    #       enable this functionality.)
    #     '';
    #   };

    #   dark = mkOption {
    #     type = with types; attrsOf str;
    #     default = { };
    #     description = ''
    #       Kitty color settings for dark background colorscheme.
    #     '';
    #   };

    #   light = mkOption {
    #     type = with types; attrsOf str;
    #     default = { };
    #     description = ''
    #       Kitty color settings for light background colorscheme.
    #     '';
    #   };

    #   common = mkOption {
    #     type = with types; attrsOf str;
    #     default = { };
    #     description = ''
    #       Kitty color settings that the light and dark background colorschemes share.
    #     '';
    #   };

    #   default = mkOption {
    #     type = types.enum [ "dark" "light" ];
    #     default = "dark";
    #     description = ''
    #       The colorscheme Kitty opens with.
    #     '';
    #   };
    # };
  };
}

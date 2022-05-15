{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.light.enable = true;

  services.xserver.libinput.touchpad.tapping = true;

  # TODO: move some of this to keyboard profile?
  # services.actkbd = {
  #   enable = true;
  #   bindings = map (x:
  #     x // {
  #       events = [ "key" ];
  #       attributes = [ "exec" ];
  #     }) [
  #       {
  #         keys = [ 225 ];
  #         command = "${pkgs.light}/bin/light -A 10";
  #       }
  #       {
  #         keys = [ 224 ];
  #         command = "${pkgs.light}/bin/light -U 10";
  #       }
  #       {
  #         keys = [ 431 ];
  #         command = (toString (pkgs.writeTextFile {
  #           name = "dark-script";
  #           text = ''
  #             if [[ `${pkgs.light}/bin/light` -eq 0 ]]
  #             then
  #               ${pkgs.light}/bin/light -I
  #             else
  #               ${pkgs.light}/bin/light -O
  #               ${pkgs.light}/bin/light -S 0
  #             fi'';
  #           executable = true;
  #         }));
  #       }
  #     ];
  # };
}

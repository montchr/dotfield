# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{ moduleWithSystem, ... }:
{
  aspects.graphical.nixos = moduleWithSystem (
    perSystem@{ config, ... }:
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.font-manager ];

      nixpkgs.config.input-fonts.acceptLicense = true;

      fonts.fontconfig.enable = true;
      fonts.fontDir.enable = true;
      fonts.packages = (
        let
          baselineFonts = with pkgs; [
            corefonts
            dejavu_fonts
            input-fonts
            iosevka
            inter
            jetbrains-mono
            nerd-fonts.symbols-only
            terminus_font
          ];

          serifFonts =
            (with pkgs; [
              bodoni-moda
              charis
              cooper
              courier-prime
              drafting-mono
              libre-baskerville
              lora
              merriweather
              newcomputermodern
            ])
            ++ (with config.packages; [
              cormorant
              playfair
            ]);

          sansSerifFonts = with pkgs; [
            jost
            merriweather-sans
          ];

          monospaceFonts = with pkgs; [
            _0xproto
            aporetic
            departure-mono
            ioskeley-mono.nf
            ioskeley-mono.term-nf
          ];
        in
        baselineFonts
        ++ serifFonts
        ++ sansSerifFonts
        ++ (with pkgs; [
          atkinson-hyperlegible-mono
          atkinson-hyperlegible-next
          fira
          stix-two
        ])
      );
    }
  );

  aspects.graphical.home = {
    fonts.fontconfig.enable = true;
  };
}

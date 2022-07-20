;;; init-ui.el --- UI Customization -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 08 Feb 2022
;;
;; URL: https://github.com/montchr/dotfield/tree/main/config/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; UI customizations for Dotfield.
;;
;;; Sources:
;;
;; https://github.com/d12frosted/environment/blob/master/emacs/lisp/init-ui.el
;;
;;; Code:


;; Bleep the beep.
(setq visible-bell nil)

(setq-default
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 use-file-dialog nil
 use-dialog-box nil)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

(hl-line-mode 1)
(blink-cursor-mode 1)

(load-theme 'modus-vivendi t)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package minions
  :init
  (minions-mode 1))

(use-package fontaine
  :commands (fontaine-store-latest-preset)
  :hook (kill-emacs-hook fontaine-store-latest-preset))

(setq fontaine-latest-state-file
      (expand-file-name "fontaine-latest-state.eld" path-cache-dir))

(setq fontaine-presets
      '((regular
          :default-height 120)
        (large
         :default-height 150)
        (t
          :default-family "Iosevka Seadome"
          :default-weight regular
          :default-height 100
          :fixed-pitch-family nil
          :fixed-pitch-family nil
          :fixed-pitch-height 1.0
          :fixed-pitch-serif-family nil
          :fixed-pitch-serif-weight nil
          :variable-pitch-family "IBM Plex Sans"
          :variable-pitch-weight nil
          :variable-pitch-height 1.0
          :bold-family nil
          :bold-weight bold
          :italic-family nil
          :italic-slant italic
          :line-spacing nil)))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

(provide 'init-ui)
;;; init-ui.el ends here.

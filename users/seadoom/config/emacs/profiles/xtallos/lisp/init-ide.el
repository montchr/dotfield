;;; init-ide.el --- IDE configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 20 Jul 2022
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
;; Configuration for an integrated development environment with LSP.
;;
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "SPC c")
  (setq lsp-use-plists t)
  (setq-default lsp-session-file (concat path-etc-dir "lsp-session"))
  (setq-default lsp-keep-workspace-alive nil)

  :commands (lsp lsp-deferred)

  :hook ((nix-mode . lsp-deferred)
         (lsp-mode . lsp-lens-mode)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(provide 'init-ide)
;;; init-ide.el ends here

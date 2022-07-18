;;; init-editor.el --- Editor configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 06 Feb 2022
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
;; Editor customizations for Dotfield.
;;
;;; Sources:
;;
;; https://github.com/d12frosted/environment/blob/master/emacs/lisp/init-editor.el
;;
;;; Code:

(require 'config-editor)

;; Backup files sound useful but get in the way, experts say.
(setq make-backup-files nil)

;; Search improvements.
(setq-default
 search-default-mode #'char-fold-to-regexp
 replace-char-fold t)

;; Whitespace settings.
(setq-default
 indent-tabs-mode nil
 tab-width xtallos/indent-width
 require-final-newline t
 tab-always-indent t)

;; Replace active region upon input.
(delete-selection-mode +1)
;; Enable column numbers.
(column-number-mode +1)

;; Refresh buffers every so often in case a file changes outside of Emacs.
(use-package autorevert
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package paren
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

(use-package visual-fill-column
  :hook ((visual-line-mode . visual-fill-column-mode)))

(use-package adaptive-wrap
  :defer t)

(use-package move-text
  :commands (move-text-up
             move-text-down)
  :bind
  (([M-S-down] . #'move-text-down)
   ([M-S-up] . #'move-text-up)))


(provide 'init-editor)
;;; init-editor.el ends here

;;; init-vcs.el --- VCS customization -*- lexical-binding: t; -*-
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
;; VCS customizations.
;;
;;; Sources:
;;
;; https://github.com/d12frosted/environment/blob/master/emacs/lisp/init-vcs.el
;;
;;; Code:

(use-package magit
  :defer t

  :defines (magit-status-mode-map
            magit-revisition-show-gravatars
            magit-display-buffer-function
            magit-diff-refine-hunk)

  :commands (magit-stage-file
             magit-unstage-file)

  :init
  (setq-default magit-git-executable (executable-find "git"))

  :config

  ;; Properly kill leftover magit buffers on quit.
  (define-key magit-status-mode-map
    [remap magit-mode-bury-buffer]
    #'vcs-quit)

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        ;; Show word-granularity on selected hunk.
        magit-diff-refine-hunk t))

(provide 'init-vcs)
;;; init-vcs.el ends here.

;;; init-elpa.el --- Initialize ELPA -*- lexical-binding: t; -*-
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
;; Setup Emacs for installing packages from MELPA and Git
;; repositories. Enable configuration via `use-package'.
;;
;;; Code:

(require 'config-path)

(setq package-user-dir
      (expand-file-name
       "elpa/"
       path-packages-dir))

;; Initialize `straight.el' {{

(setq-default
 straight-repository-branch "develop"

 ;; We aren't modifying package source code, so don't bother checking.
 straight-check-for-modifications nil
 
 ;; Make `use-package' invoke `straight.el' to install packages.
 ;; This eliminates the need to specify =:straight t= with each
 ;; invocation of `use-package'.
 straight-use-package-by-default t

 ;; Keep `straight.el' files and package repos out of the user directory.
 straight-base-dir path-packages-dir)

;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" path-packages-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; }}


;; Initialize `use-package' {{

(straight-use-package 'use-package)

(use-package el-patch
  :straight t)

;; }}


;; Common packages {{

(use-package s)
(use-package dash)
(use-package async)
(use-package request)

;; }}


;; Miscellaneous core packages {{



;; }}


(provide 'init-elpa)
;;; init-elpa.el ends here

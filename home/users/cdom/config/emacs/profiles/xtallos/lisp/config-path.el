;;; config-path.el --- Path constants -*- lexical-binding: t; -*-
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
;; This module defines path constants used across other modules.
;;
;; Note that a few similar variables are defined in `early-init.el'.
;;
;;; Code:


(defconst path-autoloads-file
  (expand-file-name "lisp/init-autoloads.el" path-emacs-dir)
  "The path to personal autoloads file.")


;;(defconst path-emacs-private-dir
;;  (concat
;;   (file-name-as-directory
;;    (concat path-home-dir ".private"))
;;   "emacs/")
;;  "The root directory for private configurations.")


(defconst path-projects-dir
  (file-name-as-directory
   (or (getenv "PROJECTS_HOME")
       (concat path-home-dir "Developer")))
  "The root directory for projects.")

(provide 'config-path)
;;; config-path.el ends here

;;; early-init.el --- Early customization -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;;
;; Created: 05 Feb 2022
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
;; See Emacs Help for more information on The Early Init File.
;; Basically, this file contains frame customizations.
;;
;;; Code:

;;
;;; Configuration paths.

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defconst path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst path-emacs-dir
  (file-name-as-directory user-emacs-directory)
  "The path to this Emacs directory.")

(defconst path-local-dir
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat path-home-dir ".cache")))
   "emacs/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defconst path-etc-dir (concat path-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst path-cache-dir (concat path-local-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files.")

(defconst path-packages-dir
  (expand-file-name (format "packages/%s.%s/"
                            emacs-major-version
                            emacs-minor-version)
                    path-local-dir)
  "Where packages are stored.")


;;; Add lisp directory to load path --------------------------------------------

(let ((emacs-home (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
                      (expand-file-name "emacs/" xdg)
                    user-emacs-directory)))
  ;; Add Lisp directory to `load-path'.
  (add-to-list 'load-path (expand-file-name "lisp" emacs-home)))


;;; Native compilation settings ------------------------------------------------

(setq native-comp-async-report-warnings-errors nil
      native-comp-deferred-compilation t)

;; Set the right directory to store the native compilation cache.
(when (fboundp 'startup-redirect-eln-cache)
  (let ((eln-cache-dir (convert-standard-filename (expand-file-name "eln-cache/" path-cache-dir))))
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path eln-cache-dir)
      (startup-redirect-eln-cache eln-cache-dir))))


;;; Misc. settings -------------------------------------------------------------


;; Disable titlebar on frames
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/433#issuecomment-1025547880
(add-to-list 'default-frame-alist '(undecorated . t))

;; Prevent `package.el' from loading packages before `straight.el' can.
;; https://github.com/raxod502/straight.el/#getting-started
(setq package-enable-at-startup nil)

;; Unicode
(set-language-environment   'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(setq locale-coding-system     'utf-8)

(provide 'early-init)
;;; early-init.el ends here

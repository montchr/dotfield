;;; init.el --- Main Configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
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
;; Emacs customizations for Dotfield.
;;
;;; Code:

(setq user-emacs-directory (file-name-directory (or (buffer-file-name) load-file-name)))

;; Add Lisp directory to `load-path'.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Delay garbage collection on startup, then re-enable.
;;
;; Setting a high gc threshold permanently can result in a perceived
;; performance improvement... until the threshold is reached, at which
;; point garbage collection can take a very long time.
;;
;; This technique can also be useful for other resource-intensive
;; operations which may benefit from a temporary lifting of the
;; gc threshold.
;;
;; https://github.com/matthewbauer/bauer#increase-gc
;;
;; Also see the original blog post which inspired the technique. If we
;; need to apply the threshold switching to numerous operations, then
;; it may be worth moving this to a function.
;;
;; http://web.archive.org/web/20210316200425/http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defvar file-name-handler-alist-backup
        file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'after-init-hook
  (lambda ()
    (garbage-collect)
    (setq gc-cons-threshold
            (car (get 'gc-cons-threshold 'standard-value))
      file-name-handler-alist
        (append
          file-name-handler-alist-backup
          file-name-handler-alist))))


;;; Bootstrap {{

(require 'config-path)
(require 'init-elpa)

;; Configure and load the customize file
;; Because sometimes we just need Emacs to write code for us
(setq custom-file (expand-file-name "custom-settings.el" user-emacs-directory))
(load custom-file t)

;;; }}


;;; Core {{

;; Environmental
(defconst xtallos/env--graphic-p (display-graphic-p))
(defconst xtallos/env--rootp (string-equal "root" (getenv "USER")))
(defconst xtallos/env-sys-mac-p (eq system-type 'darwin))
(defconst xtallos/env-sys-linux-p (eq system-type 'gnu/linux))
(defconst xtallos/env-sys-name (system-name))

;; Keys

(use-package general
  :commands (general-define-key))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;; }}


;;; Editor {{

(setq-default
 indent-tabs-mode nil
 tab-width 2
 require-final-newline t
 tab-always-indent t)

;;; }}


(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell nil)          ; No flashing visual bells

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

(hl-line-mode 1)
(blink-cursor-mode 1)

(load-theme 'modus-vivendi t)

(use-package minions
  :init
  (minions-mode 1))

(provide 'init)
;;; init.el ends here

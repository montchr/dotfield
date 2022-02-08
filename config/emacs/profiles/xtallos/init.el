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


;;
;;; Bootstrap

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

(require 'config-path)
(require 'init-elpa)

;; Configure and load the customize file
;; Because sometimes we just need Emacs to write code for us
(setq custom-file (expand-file-name "custom-settings.el" user-emacs-directory))
(load custom-file t)

(setq default-directory (concat (getenv "DOTFIELD_DIR") "/config/emacs/profiles/xtallos/"))


;;
;;; Environment

(defconst xtallos/env--graphic-p (display-graphic-p))
(defconst xtallos/env--rootp (string-equal "root" (getenv "USER")))
(defconst xtallos/env-sys-mac-p (eq system-type 'darwin))
(defconst xtallos/env-sys-linux-p (eq system-type 'gnu/linux))
(defconst xtallos/env-sys-name (system-name))
(defconst xtallos/is-darwin xtallos/env-sys-mac-p)
(defconst xtallos/is-linux xtallos/env-sys-linux-p)

(require 'init-exec-path)

;;
;;; Performance

;; Disable an unnecessary second pass over `auto-mode-alist'.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable rendering of cursors or regions in windows other than the
;; window which is currently focused.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

;; Avoid pinging random domain names.
(setq ffap-machine-p-known 'reject)

;; Disable frame resizing when changing font.
(setq frame-inhibit-implied-resize t)

;; Increase chunk size when reading from process output.
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Remove unnecessary OS-specific command-line options while running
;; Emacs in a different OS.
(unless xtallos/is-darwin    (setq command-line-ns-option-alist nil))
(unless xtallos/is-linux     (setq command-line-x-option-alist nil))


;;
;;; Core

(require 'init-keys)
(require 'init-editor)
(require 'init-ui)
(require 'init-window)

;;
;;; Utilities

(require 'init-vcs)

;;
;;; `dired'

(setq delete-by-moving-to-trash t)

;; Delete intermediate buffers while navigating.
(eval-after-load "dired"
  #'(lambda ()
      (put 'dired-find-alternate-file 'disabled nil)
      (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))


(provide 'init)
;;; init.el ends here

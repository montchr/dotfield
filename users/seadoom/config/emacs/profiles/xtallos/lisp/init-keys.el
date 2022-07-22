;;; init-keys.el --- Keybindings configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Chris Montgomery <chris@cdom.io>
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 07 Feb 2022
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
;; Configuration for keybindings.
;;
;; FIXME: M-SPC seems to be captured by GNOME
;;
;;; Code:

;;; --- configuration -----------------------------------------------------------

;; Default indentation width is 2 spaces.
(defvar xtallos/indent-width 2)


;;; --- bindings: init ---------------------------------------------------------

(defvar xtallos/leader--map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

(use-package general)

(general-create-definer xtallos/leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :prefix-map 'xtallos/leader--map
    :states '(normal insert visual emacs)
    :keymaps 'override)

  (general-create-definer xtallos/local-leader-def
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    :states '(normal insert visual emacs)
:keymaps 'override)


;;; --- bindings: top-level scopes ----------------------------------------------

(xtallos/leader-def
  ;; (to be implemented)
  "i" '(nil :wk "ins...")
  "n" '(nil :wk "note...")
  "o" '(nil :wk "open...")

  ;; --- prev/next ---
  ;; FIXME: should not be under leader map!

  "[" '(nil :wk "prev...")
  "[b" 'previous-buffer

  "]" '(nil :wk "next...")
  "]b" 'next-buffer

  ;; --- buffer ---

  "b" '(nil :wk "buff...")

  ;; buffer nav
  "bb" 'consult-project-buffer
  "bB" 'consult-buffer
  "bh" 'previous-buffer
  "b[" 'previous-buffer
  "bl" 'next-buffer
  "b]" 'next-buffer

  ;; buffer management
  "bk" 'kill-buffer
  "bs" 'save-buffer

  ;; --- eval ---

  "e" '(nil :wk "eval...")
  "eb" 'eval-buffer
  "ee" 'eval-buffer

  ;; --- file ---

  "f" '(nil :wk "file...")
  "ff" 'find-file
  "fD" 'delete-file
  "fR" 'rename-visited-file

  ;; --- git ---

  "g" '(nil :wk "git...")
  "gg" 'magit-status

  ;; --- help ---

  "h" '(nil :wk "help...")
  "hb" 'describe-bindings
  "hD" 'devdocs-lookup
  "hh" 'help
  "hf" 'describe-function
  "ho" 'describe-symbol
  "hv" 'describe-variable

  ;; --- jump around ---

  "j" '(nil :wk "jump...")
  "jj" '(avy-goto-char :which-key "char")
  "jJ" '(avy-goto-char-timer :which-key "charS")

  "jl" '(avy-goto-line :which-key "line")
  "jw" '(avy-goto-word-0 :which-key "word")
  "jb" '(ace-link :which-key "btn")

  ;; --- meta ---

  "q" '(nil :wk "meta...")
  "qf" 'delete-frame
  "qq" 'save-buffers-kill-emacs

  ;; --- search ---

  "s" '(nil :wk "serx...")
  "sp" 'consult-ripgrep
  "ss" 'consult-line

  ;; --- window management ---

  "w" '(nil :wk "wind...")

  ;; window nav
  "wh" 'windmove-left
  "wH" 'windmove-swap-states-left
  "wj" 'windmove-down
  "wJ" 'windmove-swap-states-down
  "wk" 'windmove-up
  "wK" 'windmove-swap-states-up
  "wl" 'windmove-right
  "wL" 'windmove-swap-states-right

  "wn" 'split-window-vertically
  "wN" 'split-window-horizontally
  "wd" 'delete-window)


;;; --- bindings: top-level oneoffs ---------------------------------------------

(xtallos/leader-def
  "a"   'org-agenda
  ;; FIXME: possible conflict with lsp-mode?
  ;; "c"   'org-capture
  "y"   'consult-yank-pop

  "-"   'calendar
  "="   'quick-calc
  "+"   'calc

  ;; "S-<return>" 'vterm

  "SPC" 'project-find-file)

;;
;;; Keybinding UX

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        ;; which-key-max-display-columns nil
        which-key-min-display-lines 6))

;; <escape> escaping

(defvar kbd-escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.
Triggers `kbd-escape'.
If any hook returns non-nil, all hooks after it are ignored.")

(defun kbd-escape ()
  "Run the `kbd-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop
        ;; there.
        ((cl-find-if #'funcall kbd-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'kbd-escape)

;; (when (and env-sys-mac-p env-graphic-p)
;;   (defvar mac-option-modifier)
;;   (defvar mac-command-modifier)
;;   (setq mac-option-modifier nil
;;         mac-command-modifier 'meta))

(provide 'init-keys)
;;; init-keys.el ends here

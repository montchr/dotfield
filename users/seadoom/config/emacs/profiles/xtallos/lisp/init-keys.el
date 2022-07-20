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
;; Configuration for keybindings, evil-mode, and its evil relatives.
;;
;;; Code:

(require 'config-editor)

(use-package evil
  ;; :after (minions)
  :diminish undo-tree-mode

  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width xtallos/indent-width)

  :hook (after-init . evil-mode)

  :preface
  (defun xtallos/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'xtallos/save-and-kill-this-buffer))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package general)

  ;; :config
  ;; (general-define-key
  ;;  :prefix "SPC"
  ;;  :keymaps 'override
  ;;  :states '(normal visual motion)

  ;;  "<left>" 'previous-buffer
  ;;  "<right>" 'next-buffer

  ;;  "a" 'org-agenda
  ;;  "bb" 'consult-buffer
  ;;  ;; "c" 'org-capture
  ;;  "ff" 'find-file
  ;;  "gg" 'magit-status
  ;;  "sp" 'project-find-file
  ;;  "ss" 'consult-line
  ;;  "w" 'write-file
  ;;  "y" 'consult-yank-pop
   
  ;;  "S-<return>" 'vterm
  ;;  "-" 'calendar
  ;;  "=" 'quick-calc
  ;;  "+" 'calc
  ;;  "," 'eww))

(general-create-definer xtallos/leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer xtallos/local-leader-def
  :prefix "SPC m"
  :non-normal-prefix "M-SPC m")

(xtallos/leader-def
 "/" '(nil :which-key "Find")
 "[" '(nil :which-key "Prev")
 "g" '(nil :which-key "Git")
 "i" '(nil :which-key "Ins")
 "j" '(nil :which-key "Jump")
 "n" '(nil :which-key "Note")
 "o" '(nil :which-key "Open"))

(use-package bind-key)

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

(when (and env-sys-mac-p env-graphic-p)
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))


(provide 'init-keys)
;;; init-keys.el ends here

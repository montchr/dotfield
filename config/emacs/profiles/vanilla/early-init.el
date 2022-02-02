;;; early-init.el --- Dotfield Early Initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Chris Montgomery
;;
;; Author: Chris Montgomery <https://github.com/montchr>
;; Maintainer: Chris Montgomery <chris@cdom.io>
;; Created: February 02, 2022
;; Modified: February 02, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/montchr/early-init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Dotfield Early Initialization
;;
;;; Code:

;; Disable titlebar on frames.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/433#issuecomment-1025547880
(add-to-list 'default-frame-alist '(undecorated . t))

(provide 'early-init)
;;; early-init.el ends here

;;; completion/corfu/autoload/corfu.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion corfu +minibuffer)
;;
;; Copyright 2022, Chris Montgomery
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright 2022, Ellis Kenyő
;; https://github.com/elken/doom/blob/55c1c70fc89368211f3adcf1bad80de70b704d74/modules/completion/corfu/autoload/corfu.el
;; SPDX-License-Identifier: MIT

;;;###autoload
(defun +corfu--enable-in-minibuffer ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex
                                   evil-ex-search-forward
                                   evil-ex-search-backward))
              (and (featurep! :completion helm)
                   (helm--alive-p))
              (corfu-mode +1))))

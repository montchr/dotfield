;;; completion/corfu/config.el -*- lexical-binding: t; -*-
;;
;; Copyright 2022, Chris Montgomery
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright 2022, Ellis Kenyő
;; https://github.com/elken/doom/blob/55c1c70fc89368211f3adcf1bad80de70b704d74/modules/completion/corfu/config.el
;; SPDX-License-Identifier: MIT
;;
;; Copyright 2020 - 2022, Gerry Agbobada
;; https://git.sr.ht/~gagbo/doom-config/tree/31d0bd649b3eb97aebc319088e2674b0412e2beb/item/modules/completion/corfu/config.el
;; SPDX-License-Identifier: MIT

;; Set bindings
(map! :i "C-@" #'completion-at-point
      :i "C-SPC" #'completion-at-point
      (:prefix "C-x"
       :i "C-k" #'cape-dict
       :i "C-f" #'cape-file
       :i "s" #'cape-ispell
       :i "C-n" #'corfu-next
       :i "C-p" #'corfu-previous
       :i "C-s" #'dabbrev-completion))

(use-package! corfu
  :custom
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (corfu-cycle nil)
  (corfu-scroll-margin 4)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  :hook
  (doom-first-buffer . global-corfu-mode)
  :config
  (when (featurep! +minibuffer)
    (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer))

  ;; Dirty hack to get c completion running
  ;; Discussion in https://github.com/minad/corfu/issues/34
  (when (and (featurep! :lang cc)
             (equal tab-always-indent 'complete))
    (map! :map c-mode-base-map
          :i [remap c-indent-line-or-region] #'completion-at-point))

  ;; Reset lsp-completion provider
  (add-hook 'doom-init-modules-hook
            (lambda ()
              (after! lsp-mode
                (setq lsp-completion-provider :none))))

  ;; Set orderless filtering for LSP-mode completions
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless flex))))))
  (after! evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)
    (corfu-terminal-mode))
  (map! :map corfu-map
        "C-SPC"    #'corfu-insert-separator
        "TAB"      #'corfu-next
        [tab]      #'corfu-next
        "C-n"      #'corfu-next
        "C-p"      #'corfu-previous
        "S-TAB"    #'corfu-previous
        [backtab]  #'corfu-previous
        (:prefix "C-x"
         "C-k"     #'cape-dict
         "C-f"     #'cape-file)))

(use-package! corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0)
  :bind (:map corfu-map
         ("M-n" . corfu-doc-scroll-down)
         ("M-p" . corfu-doc-scroll-up)
         ("M-d" . corfu-doc-toggle)))

(use-package! orderless
  :when (featurep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package! kind-icon
  :after corfu
  :when (featurep! +icons)
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (setq kind-icon-use-icons t
        kind-icon-default-face 'corfu-default
        svg-lib-icons-dir (expand-file-name "svg-lib" doom-cache-dir))
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! cape
  :defer t
  :init
  (map!
   [remap dabbrev-expand] 'cape-dabbrev)
  (add-hook! 'latex-mode-hook (defun +corfu--latex-set-capfs ()
                                (add-to-list 'completion-at-point-functions #'cape-tex)))
  (when (featurep! :checkers spell)
    (setq cape-dict-file ispell-personal-dictionary)
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-ispell))
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

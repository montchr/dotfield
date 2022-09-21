;;; $DOOMDIR/graveyard/graveyard.el -*- lexical-binding: t; -*-

;; (use-package! embark-vc
;;   :after embark)

;; Keep the minibuffer in a different frame (only for `read-from-minibuffer').
;; via https://github.com/elken/doom/commit/765563e089b8b92bd21368100b84e042edb9c529
;; FIXME: only when gtk feature is available (29+)
;; (use-package! mini-frame
;;   :hook (doom-init-ui-hook . mini-frame-mode)
;;   :init
;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 10)
;;        (width . 1.0)
;;        (left . 0.5)
;;        (no-accept-focus . t))))
;;   :config
;;   ;; Workaround for GNOME Shell compatibility.
;;   ;; https://github.com/muffinmad/emacs-mini-frame#gnome-shell-does-not-resize-emacs-child-frames
;;   (when (string= (getenv "XDG_SESSION_DESKTOP") "gnome")
;;     (setq x-gtk-resize-child-frames 'resize-mode)))



;; Store the value of the shell environment's =SSH_*= variables when generating
;; the env file.
;;
;; FIXME: results in error. the name of the doom variable has likely changed
;; upstream in 3.0.0 prep.
;;
;; (when noninteractive (add-to-list
;; 'doom-env-whitelist "^SSH_"))

;; TODO: these don't work quite right
;; https://github.com/konrad1977/emacs/blob/main/init.el
;; (use-package! svg-tag-mode
;;   :hook ((prog-mode . svg-tag-mode)
;;          (org-mode . svg-tag-mode))
;;   :config
;;   (setq svg-tag-tags
;;         '(
;;           ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
;;           ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0))))
;;           ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
;;           ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

;;           ("\\/\\/\\W?eslint-disable" . ((lambda (tag) (svg-tag-make "eslint-disable" :face 'org-level-3 :inverse t :margin 0 :crop-right t))))
;;           ("eslint-disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-3 :crop-left t))))

;;           ("\\/\\/\\W?TODO\\b\\|TODO\\b" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :crop-right t))))
;;           ("TODO\\b\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
;;           )))


;; Add a CREATED property to org-mode headings.
;; (use-package! org-expiry
;;   :after (org)
;;   :config
;;   (setq! org-expiry-inactive-timestamps t)
;;   (org-expiry-insinuate))

;;; config/emacs/profiles/doom/graveyard.el -*- lexical-binding: t; -*-

;; === graveyard ===============================================================

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

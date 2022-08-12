;;; completion/corfu/autoload/minad-capf.el -*- lexical-binding: t; -*-
;;
;; Copyright 2020 - 2022, Gerry Agbobada
;; https://git.sr.ht/~gagbo/doom-config/tree/31d0bd649b3eb97aebc319088e2674b0412e2beb/item/modules/completion/corfu/autoload/minad-capf.el
;; SPDX-License-Identifier: MIT
;;
;; Copyright 2021, Daniel Mendler
;; https://github.com/minad/corfu/issues/9#issuecomment-945090516
;; SPDX-License-Identifier: GPL-3.0-only

(require 'dabbrev)

;;;###autoload
(defun +file-completion-at-point-function ()
  "File name completion-at-point-function."
  (when-let (bounds (bounds-of-thing-at-point 'filename))
    (list (car bounds) (cdr bounds)
          'read-file-name-internal
          :exclusive 'no
          :annotation-function (lambda (_) " (File)"))))

;;;###autoload
(defun +dabbrev-completion-at-point-function ()
  (let ((dabbrev-check-all-buffers nil)
        (dabbrev-check-other-buffers nil))
    (dabbrev--reset-global-variables))
  (let ((abbrev (ignore-errors (dabbrev--abbrev-at-point))))
    (when (and abbrev (not (string-match-p "[ \t]" abbrev)))
      (pcase ;; Interruptible scanning
          (while-no-input
            (let ((inhibit-message t)
                  (message-log-max nil))
              (or (dabbrev--find-all-expansions
                   abbrev (dabbrev--ignore-case-p abbrev))
                  t)))
        ('nil (keyboard-quit))
        ('t nil)
        (words
         ;; Ignore completions which are too short
         (let ((min-len (+ 4 (length abbrev))))
           (setq words (seq-remove (lambda (x) (< (length x) min-len)) words)))
         (when words
           (let ((beg (progn (search-backward abbrev) (point)))
                 (end (progn (search-forward abbrev) (point))))
             (unless (string-match-p "\n" (buffer-substring beg end))
               (list beg end words
                     :exclusive 'no
                     :annotation-function (lambda (_) " (Dabbrev)"))))))))))

(autoload 'ispell-lookup-words "ispell")

;;;###autoload
(defun +ispell-completion-at-point-function ()
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (table (with-demoted-errors
                         (let ((message-log-max nil)
                               (inhibit-message t))
                           (ispell-lookup-words
                            (format "*%s*"
                                    (buffer-substring-no-properties (car bounds) (cdr bounds))))))))
    (list (car bounds) (cdr bounds) table
          :exclusive 'no
          :annotation-function (lambda (_) " (Ispell)"))))

(defun +word-completion-at-point-function (words)
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds) words
          :exclusive 'no
          :annotation-function (lambda (_) " (Words)"))))

(defvar +dict--words nil)
(defvar +dict-file "/etc/dictionaries-common/words")

;;;###autoload
(defun +dict-completion-at-point-function ()
  (+word-completion-at-point-function
   (or +dict--words
       (setq +dict--words
             (split-string (with-temp-buffer
                             (insert-file-contents-literally +dict-file)
                             (buffer-string))
                           "\n")))))

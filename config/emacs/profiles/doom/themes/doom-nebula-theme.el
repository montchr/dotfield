;; doom-nebula-theme.el --- inspired from the doom one and modus themes -*- no-byte-compile: t; -*-

;; Author: Pete Kazmier
;; URL: https://github.com/pkazmier/doom-nebula-theme

;; Copyright (c) 2020 Pete Kazmier

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'doom-themes)

;;; Code:
(defgroup doom-nebula-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-nebula-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nebula-theme
  :type 'boolean)

(defcustom doom-nebula-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line."
  :group 'doom-nebula-theme
  :type '(choice integer boolean))

(defcustom doom-nebula-variable-font-weight 'light
  "If non-nil, weight used to style variable pitch headings."
  :group 'doom-nebula-theme
  :type 'symbol)

(defcustom doom-nebula-variable-font-height-multiplier 1.1
  "If non-nil, height multiplier used to style variable pitch headings."
  :group 'doom-nebula-theme
  :type 'float)

(def-doom-theme doom-nebula
  "A dark theme based on doom-one and modus themes."

;; name           gui       256       16
  ((bg         '("#101114" nil       nil))
   (bg-alt     '("#14161a" nil       nil))
   (base0      '("#181a1f" "#101010" "black"        ))
   (base1      '("#1b1e24" "#21212d" "brightblack"  ))
   (base2      '("#1f2329" "#21212d" "brightblack"  ))
   (base3      '("#23272e" "#1e1e1e" "brightblack"  ))
   (base4      '("#45484D" "#5e5e5e" "brightblack"  ))
   (base5      '("#5e6269" "#666666" "brightblack"  ))
   (base6      '("#757a82" "#7b7b7b" "brightblack"  ))
   (base7      '("#9ca2ad" "#979797" "brightblack"  ))
   (base8      '("#FFFFFF" "#dfdfdf" "white"        ))
   (fg         '("#cbd2df" "#bfbfbf" ))
   (fg-alt     '("#5e6269" "#5d5d5d" ))

   (grey       base4)
   (red        '("#ff6c6b" "#ff6655" ))
   (orange     '("#fd8B39" "#dd8844" ))
   (green      '("#79CF72" "#99bb66" ))
   (yellow     '("#FCCB60"           ))
   (blue       '("#51afff"           ))
   (dark-blue  '("#005F88" "#2257A0" "blue"         ))
   (magenta    '("#bf64da"           ))
   (violet     '("#A085F2"           ))
   (cyan       '("#44DAEB"           ))
   (dark-cyan  '("#00acbf"           ))
   (teal       '("#00b594" "#44b9b1" ))

   ;; face categories
   (region         base4)
   (highlight      blue)
   (vertical-bar   fg-alt)
   (selection      dark-blue)
   (builtin        dark-cyan)
   (comments       base5)
   (doc-comments   base6)
   (doc-comments   dark-blue)
   (constants      magenta)
   (functions      violet)
   (keywords       blue)
   ;; Not sure what color to use on methods as I can't find a programming mode
   ;; that uses this face.
   ;; (methods        violet)
   (methods        dark-cyan)
   ;; Not sure what color to use on operators as I can't find a programming mode
   ;; that uses this face.
   ;; (operators      blue)
   (operators      red)
   (type           cyan)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     (list (car bg) "black" "black"))
   (hidden-alt (list (car bg-alt) "black" "black"))

   ;; modeline settings
   (-modeline-pad
    (when doom-nebula-padded-modeline
      (if (integerp doom-nebula-padded-modeline) doom-nebula-padded-modeline 4)))

   (modeline-fg fg)
   (modeline-fg-alt (doom-blend blue grey 0.08))

   ;; Active modeline background should be consistent regardless of solaire mode
   ;; or not, so it is easily identified as the active modeline.
   (modeline-bg  (if doom-nebula-brighter-modeline base4 base3))
   (modeline-bg-l (if doom-nebula-brighter-modeline base4 base3))

   ;; Inactive modeline background should match the background of window so
   ;; there is no noticeable modeline.
   (modeline-bg-inactive   (cons (car bg-alt) (cdr base0)))
   (modeline-bg-inactive-l (cons (car bg) (cdr base0))))

  ;; --- extra faces ------------------------
  (((all-the-icons-dblue &override) :foreground dark-cyan)

   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :inherit 'font-lock-comment-face :slant 'normal :foreground doc-comments)
   (font-lock-preprocessor-face :foreground dark-cyan)

   ;; I didn't like the background on non-active lines as it competes with the
   ;; window borders, which are designed to be very clear.
   ;; ((line-number &override) :background base0 :foreground base5)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :background base3 :foreground blue :bold bold)

   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-path :foreground blue :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; helpful
   (helpful-heading :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.20) :weight doom-nebula-variable-font-weight :foreground cyan)

   ;; links & info
   (link-visited :foreground comments)
   (info-menu-star :foreground magenta)
   (info-menu-header :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.20) :weight doom-nebula-variable-font-weight :foreground cyan)
   (info-title-1 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.33) :weight doom-nebula-variable-font-weight :foreground dark-cyan)
   (info-title-2 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.27) :weight doom-nebula-variable-font-weight :foreground teal)
   (info-title-3 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.21) :weight doom-nebula-variable-font-weight :foreground violet)
   (info-title-4 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.15) :weight doom-nebula-variable-font-weight :foreground magenta)

   ;; highlight
   (lazy-highlight :background (doom-darken dark-blue 0.2) :foreground (doom-lighten cyan 0.3) :weight 'bold)
   (lsp-face-highlight-textual :underline base8 :weight 'bold)
   (lsp-face-highlight-read    :underline base8 :weight 'bold)
   (lsp-face-highlight-write   :underline base8 :weight 'bold)

   ;; tooltip
   ;; (tooltip :background base0)

   ;; company
   (company-tooltip-selection :background selection :weight 'bold)

   ;; magit
   (magit-diff-hunk-heading :background base3 :foreground comments)
   (magit-diff-hunk-heading-highlight :background base3 :foreground dark-cyan :weight 'bold :extend t)

   ;; markdown
   (markdown-list-face :foreground blue)
   (markdown-markup-face :foreground base5)
   (markdown-header-delimiter-face :inherit 'variable-pitch :foreground comments)
   (markdown-header-face-1 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.5) :weight doom-nebula-variable-font-weight :foreground blue)
   (markdown-header-face-2 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.33) :weight doom-nebula-variable-font-weight :foreground dark-cyan)
   (markdown-header-face-3 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.27) :weight doom-nebula-variable-font-weight :foreground teal)
   (markdown-header-face-4 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.21) :weight doom-nebula-variable-font-weight :foreground violet)
   (markdown-header-face-5 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.15) :weight doom-nebula-variable-font-weight :foreground magenta)
   (markdown-header-face-6 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.1)  :weight doom-nebula-variable-font-weight :foreground cyan)
   (markdown-header-face-7 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.0)  :weight doom-nebula-variable-font-weight :foreground green)
   (markdown-header-face-8 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.0)  :weight doom-nebula-variable-font-weight :foreground yellow)
   ((markdown-code-face &override) :background base1)

   ;; mu4e
   (mu4e-highlight-face :foreground blue :weight 'bold)
   (mu4e-header-highlight-face :background (doom-darken dark-blue 0.2) :foreground (doom-lighten cyan   0.3) :weight 'bold)

   ;; snipe
   (evil-snipe-first-match-face :foreground highlight :background dark-blue :weight 'bold)
   (evil-snipe-matches-face :background (doom-darken dark-cyan 0.5) :foreground (doom-lighten green 0.6) :weight 'bold)

   ;; ivy-mode
   (ivy-posframe :background base0)
   (ivy-posframe-border :background fg-alt)

   (ivy-current-match :background selection :foreground base8 :weight 'bold)
   (ivy-minibuffer-match-face-1 :background base3 :foreground comments :weight 'bold)
   (ivy-minibuffer-match-face-2 :background (doom-darken dark-blue 0.2) :foreground (doom-lighten cyan   0.3) :weight 'bold)
   (ivy-minibuffer-match-face-3 :background (doom-darken magenta   0.5) :foreground (doom-lighten violet 0.7) :weight 'bold)
   (ivy-minibuffer-match-face-4 :background (doom-darken dark-cyan 0.5) :foreground (doom-lighten green  0.6) :weight 'bold)

   ;; swiper
   (swiper-line-face :background selection :foreground base8 :weight 'bold)
   (swiper-match-face-1 :foreground base8 :weight 'bold)
   (swiper-match-face-2 :foreground (doom-lighten cyan 0.3) :weight 'bold)
   (swiper-match-face-3 :foreground (doom-lighten violet 0.7) :weight 'bold)
   (swiper-match-face-4 :foreground (doom-lighten green  0.6) :weight 'bold)

   (swiper-background-match-face-1 :background base3 :foreground comments)
   (swiper-background-match-face-2 :background (doom-darken dark-blue 0.2) :foreground (doom-lighten cyan 0.3) :weight 'bold)
   (swiper-background-match-face-3 :background (doom-darken magenta   0.5) :foreground (doom-lighten violet 0.7) :weight 'bold)
   (swiper-background-match-face-4 :background (doom-darken dark-cyan 0.5) :foreground (doom-lighten green  0.6) :weight 'bold)

   (whitespace-empty :background base2)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground violet)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)

   ;; org-mode
   (org-block                    :background base1 :extend t)
   (org-block-background         :background base1 :extend t)
   (org-meta-line                :inherit 'shadow)
   (org-block-begin-line         :foreground comments :background base1 :extend t)

   (org-document-info :foreground violet)
   (org-document-title :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.5) :weight doom-nebula-variable-font-weight :foreground blue)
   (org-level-1 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.33) :weight doom-nebula-variable-font-weight :foreground dark-cyan)
   (org-level-2 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.27) :weight doom-nebula-variable-font-weight :foreground teal)
   (org-level-3 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.21) :weight doom-nebula-variable-font-weight :foreground violet)
   (org-level-4 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.15) :weight doom-nebula-variable-font-weight :foreground magenta)
   (org-level-5 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.1)  :weight doom-nebula-variable-font-weight :foreground cyan)
   (org-level-6 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.0)  :weight doom-nebula-variable-font-weight :foreground green)
   (org-level-7 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.0)  :weight doom-nebula-variable-font-weight :foreground yellow)
   (org-level-8 :inherit 'variable-pitch :height (* doom-nebula-variable-font-height-multiplier 1.0)  :weight doom-nebula-variable-font-weight :foreground orange)

   (+org-todo-project :inherit '(bold org-todo) :foreground teal)

   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden-alt)

   ;; org-agenda
   (org-date :foreground base6)
   (org-agenda-structure :foreground highlight)
   (org-super-agenda-header :inherit 'variable-pitch :weight doom-nebula-variable-font-weight :height (* doom-nebula-variable-font-height-multiplier 1.2) :foreground dark-cyan)
   (org-agenda-date :foreground violet)
   (org-agenda-date-weekend :foreground magenta)
   (org-agenda-date-today :foreground base8 :underline t :weight 'bold)
   (org-scheduled :foreground comments)
   (org-scheduled-today :foreground fg)
   (org-warning :foreground cyan)
   (org-scheduled-previously :foreground cyan)
   (org-upcoming-deadline :foreground cyan)

   ;; Racket
   (racket-selfeval-face :foreground cyan)
   (racket-keyword-argument-face :foreground teal)

   ;; Spull-fu
   (spell-fu-incorrect-face :underline `(:style wave :color ,red))

   ;; Treemacs
   (treemacs-root-face :foreground highlight :weight 'ultra-bold :height (* doom-nebula-variable-font-height-multiplier 1.2))
   (doom-themes-treemacs-root-face :foreground highlight :weight 'ultra-bold :height (* doom-nebula-variable-font-height-multiplier 1.2))
   (doom-themes-treemacs-file-face :foreground dark-cyan)
   (treemacs-directory-face :foreground violet)

   (writegood-passive-voice-face :underline `(:style wave :color ,teal))
   )

  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-nebula-theme.el ends here

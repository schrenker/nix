;;; solarized-overlay.el --- Personal customized theme -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This file contains my own modifications to solarized theme, including color handling for theme switching.

;;; Code:
(defvar emacs-appearance 'dark
  "This variable holds initial value for theme if there is no dynamic system in place (macos), or value of theme that has been switched to.")

(defun schrenker/solarized-theme-overlay (appearance)
  "Apply collection of colors and faces to Emacs, based on solarized-theme colors and current APPEARANCE."
  (let ((yellow    "#b58900")
        (yellow-d  "#866300")
        (yellow-l  "#e1af4b")
        (orange    "#cb4b16")
        (orange-d  "#992700")
        (orange-l  "#fb7640")
        (red       "#dc322f")
        (red-d     "#a7020a")
        (red-l     "#ff6849")
        (magenta   "#d33682")
        (magenta-d "#a00559")
        (magenta-l "#ff699e")
        (violet    "#6c71c4")
        (violet-d  "#243e9b")
        (violet-l  "#8d85e7")
        (blue      "#268bd2")
        (blue-d    "#0061a8")
        (blue-l    "#74adf5")
        (cyan      "#2aa198")
        (cyan-d    "#007d76")
        (cyan-l    "#6ccec0")
        (green     "#859900")
        (green-d   "#5b7300")
        (green-l   "#b3c34d")
        (teal      "#35a69c")
        (white     "#ffffff")
        (black     "#181818")

        (bg-main      (if (eq appearance 'light) "#fdf6e3" "#002b36"))
        (bg-alt       (if (eq appearance 'light) "#eee8d5" "#073642"))
        (bg-highlight (if (eq appearance 'light) "#ece3cc" "#184956"))

        (fg-main      (if (eq appearance 'light) "#657b83" "#839496"))
        (fg-alt       (if (eq appearance 'light) "#93a1a1" "#586e75"))
        (fg-emph      (if (eq appearance 'light) "#586e75" "#93a1a1"))

        (yellow-1bg   (if (eq appearance 'light) "#f8e8c6" "#273532"))
        (yellow-1fg   (if (eq appearance 'light) "#876d26" "#af8f41"))
        (yellow-2bg   (if (eq appearance 'light) "#f1d49b" "#433e20"))
        (yellow-2fg   (if (eq appearance 'light) "#766634" "#b39a5e"))
        (orange-1bg   (if (eq appearance 'light) "#fedfc5" "#2b2d2e"))
        (orange-1fg   (if (eq appearance 'light) "#974727" "#ca6f48"))
        (orange-2bg   (if (eq appearance 'light) "#ffbd99" "#4d2c1f"))
        (orange-2fg   (if (eq appearance 'light) "#854a33" "#c47c5d"))
        (red-1bg      (if (eq appearance 'light) "#ffdec8" "#2d2c31"))
        (red-1fg      (if (eq appearance 'light) "#a33c35" "#d66556"))
        (red-2bg      (if (eq appearance 'light) "#ffb9a1" "#532725"))
        (red-2fg      (if (eq appearance 'light) "#8e433d" "#ce7667"))
        (magenta-1bg  (if (eq appearance 'light) "#fdded7" "#272d3c"))
        (magenta-1fg  (if (eq appearance 'light) "#9a3f6c" "#cc6791"))
        (magenta-2bg  (if (eq appearance 'light) "#fdbac6" "#4c2942"))
        (magenta-2fg  (if (eq appearance 'light) "#854568" "#c47896"))
        (violet-1bg   (if (eq appearance 'light) "#ebe4e2" "#0c3144"))
        (violet-1fg   (if (eq appearance 'light) "#4f5e99" "#8085c0"))
        (violet-2bg   (if (eq appearance 'light) "#d1c9e3" "#1a365a"))
        (violet-2fg   (if (eq appearance 'light) "#475a8b" "#888dbc"))
        (blue-1bg     (if (eq appearance 'light) "#e7e8e4" "#003547"))
        (blue-1fg     (if (eq appearance 'light) "#1e6fa2" "#5c93c5"))
        (blue-2bg     (if (eq appearance 'light) "#c3d5e9" "#003f5e"))
        (blue-2fg     (if (eq appearance 'light) "#246792" "#709bc3"))
        (cyan-1bg     (if (eq appearance 'light) "#e4ecda" "#013841"))
        (cyan-1fg     (if (eq appearance 'light) "#207e7b" "#54a099"))
        (cyan-2bg     (if (eq appearance 'light) "#bedfcf" "#00464a"))
        (cyan-2fg     (if (eq appearance 'light) "#247374" "#6ba8a2"))
        (green-1bg    (if (eq appearance 'light) "#efeac7" "#1d3732"))
        (green-1fg    (if (eq appearance 'light) "#657827" "#8c9a43"))
        (green-2bg    (if (eq appearance 'light) "#dbdb9c" "#2f4321"))
        (green-2fg    (if (eq appearance 'light) "#5b6e35" "#97a35f"))

        (org-box '(:weight bold :inverse-video t)))
    (progn
      (setopt org-todo-keyword-faces
              `(("NEXT"       :foreground ,yellow  ,@org-box)
                ("TODO"       :foreground ,magenta ,@org-box)
                ("INPROGRESS" :foreground ,green   ,@org-box)
                ("BLOCKED"    :foreground ,orange  ,@org-box)
                ("ONHOLD"     :foreground ,cyan    ,@org-box)
                ("REVIEW"     :foreground ,blue    ,@org-box)
                ("DONE"       :foreground ,fg-emph ,@org-box)
                ("CANCELLED"  :foreground ,fg-alt  ,@org-box)
                ("DELEGATED"  :foreground ,fg-main ,@org-box))
              org-priority-faces
              `((?A :foreground ,red     ,@org-box)
                (?B :foreground ,yellow  ,@org-box)
                (?C :foreground ,violet  ,@org-box)
                (?D :foreground ,fg-emph ,@org-box))
              org-src-block-faces
              `(("emacs-lisp" (:background ,magenta-1bg :extend t))
                ("python"     (:background ,green-1bg   :extend t))
                ("yaml"       (:background ,cyan-1bg    :extend t))
                ("json"       (:background ,blue-1bg    :extend t))
                ("bash"       (:background ,green-1bg   :extend t))
                ("sh"         (:background ,green-1bg   :extend t))
                ("shell"      (:background ,green-1bg   :extend t))
                ("fish"       (:background ,green-1bg   :extend t))
                ("nix"        (:background ,blue-1bg    :extend t)))
              org-modern-todo-faces org-todo-keyword-faces
              org-modern-priority-faces org-priority-faces)

      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
        ('light (load-theme 'solarized-light t))
        ('dark  (load-theme 'solarized-dark  t)))

      (with-eval-after-load 'kind-icon
        (kind-icon-reset-cache))

      (with-eval-after-load 'prism
        (prism-set-colors
          :num 20
          :desaturations '(0 5 10 15 20)
          :lightens '(0 -15 -30 -45 -60)
          :colors (list blue green cyan yellow)))

      (with-eval-after-load 'hl-line
        (set-face-background 'hl-line bg-alt))

      (with-eval-after-load 'diff-hl
        (set-face-attribute 'diff-hl-change nil
                            :background blue-2bg
                            :foreground (if (eq appearance 'light) blue-d blue-l))
        (set-face-attribute 'diff-hl-insert nil
                            :background green-2bg
                            :foreground (if (eq appearance 'light) green-d green-l))
        (set-face-attribute 'diff-hl-delete nil
                            :background red-2bg
                            :foreground (if (eq appearance 'light) red-d red-l)))

      (set-face-attribute 'italic nil :slant 'italic :underline nil)
      (set-face-attribute 'link nil :foreground cyan :slant 'italic )

      (set-face-attribute 'mode-line nil
                          :background blue-2bg
                          :foreground fg-emph
                          :overline blue-d
                          :underline nil
                          :box `(:line-width 1 :color ,blue-d))
      (set-face-attribute 'mode-line-active nil
                          :background blue-2bg
                          :foreground fg-emph
                          :overline blue-d
                          :underline nil
                          :box `(:line-width 1 :color ,blue-d))
      (set-face-attribute 'mode-line-inactive nil
                          :background bg-main
                          :overline bg-alt
                          :underline nil
                          :box `(:line-width 1 :color ,bg-alt))

      (set-face-attribute 'tab-bar nil
                          :background bg-alt
                          :foreground fg-main
                          :box `(:line-width 3 :color ,bg-alt :style nil))
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background bg-alt
                          :foreground fg-main
                          :box `(:line-width 3 :color ,bg-alt :style nil))
      (set-face-attribute 'tab-bar-tab nil
                          :background (if (eq appearance 'light) bg-main bg-highlight)
                          :foreground fg-emph
                          :box `(:line-width 3 :color ,(if (eq appearance 'light) bg-main bg-highlight) :style nil))

      (with-eval-after-load 'dired-subtree
        (set-face-attribute 'dired-subtree-depth-1-face nil :background bg-highlight)
        (set-face-attribute 'dired-subtree-depth-2-face nil :background bg-highlight)
        (set-face-attribute 'dired-subtree-depth-3-face nil :background bg-highlight)
        (set-face-attribute 'dired-subtree-depth-4-face nil :background bg-highlight)
        (set-face-attribute 'dired-subtree-depth-5-face nil :background bg-highlight)
        (set-face-attribute 'dired-subtree-depth-6-face nil :background bg-highlight))

      (with-eval-after-load 'org
        (set-face-attribute 'org-level-1 nil :background orange-2bg :foreground (if (eq appearance 'light) orange-d orange-l) :extend t)
        (set-face-attribute 'org-level-2 nil :background green-2bg  :foreground (if (eq appearance 'light) green-d green-l) :extend t)
        (set-face-attribute 'org-level-3 nil :background blue-2bg   :foreground (if (eq appearance 'light) blue-d blue-l) :extend t)
        (set-face-attribute 'org-level-4 nil :background yellow-2bg :foreground (if (eq appearance 'light) yellow-d yellow-l) :extend t)
        (set-face-attribute 'org-level-5 nil :background cyan-2bg   :foreground (if (eq appearance 'light) cyan-d cyan-l) :extend t)
        (set-face-attribute 'org-level-6 nil :background green-2bg  :foreground (if (eq appearance 'light) green-d green-l) :extend t)
        (set-face-attribute 'org-level-7 nil :background red-2bg    :foreground (if (eq appearance 'light) red-d red-l) :extend t)
        (set-face-attribute 'org-level-8 nil :background blue-2bg   :foreground (if (eq appearance 'light) blue-d blue-l) :extend t)
        (set-face-background 'org-block bg-alt)
        (set-face-extend 'org-block-begin-line t)
        (set-face-extend 'org-block-end-line t)
        (set-face-attribute 'org-checkbox nil :box nil :background bg-alt)

        (run-with-idle-timer 0 nil (lambda ()(mapc (lambda (buffer) (with-current-buffer buffer (when (derived-mode-p 'org-mode)(font-lock-update)))) (buffer-list)))))

      (with-eval-after-load 'org-modern
        (set-face-attribute 'org-modern-todo nil :height 1.0 :weight 'bold :box '(:line-width 1))
        (set-face-attribute 'org-modern-date-active nil :foreground fg-emph :background bg-alt)
        (set-face-attribute 'org-modern-date-inactive nil :foreground fg-alt :background bg-alt)
        (set-face-attribute 'org-modern-time-active nil :foreground fg-emph :background bg-main :inverse-video t)
        (set-face-attribute 'org-modern-time-inactive nil :foreground fg-alt :background bg-main :inverse-video t)
        (custom-set-faces `(org-modern-tag ((t (:inherit (secondary-selection org-modern-label) :weight bold :height 1.0 :foreground ,violet :inverse-video t)))))
        (custom-set-faces `(org-modern-statistics ((t (:inherit org-modern-label :weight bold :background ,bg-alt :foreground ,green)))))
        (advice-add
         'org-modern--update-faces
         :override
         (lambda (&rest r)
           (set-face-attribute 'org-modern-label nil :height 1.0 :box nil))))

      (with-eval-after-load 'ace-window
        (custom-set-faces `(aw-leading-char-face ((t (:inherit org-modern-label :width expanded :weight bold :background ,magenta :foreground ,bg-main :height 3.0 ))))))

      (with-eval-after-load 'embark
        (set-face-background 'embark-target (if (eq appearance 'light) white black)))

      (with-eval-after-load 'hydra
        (setopt hydra-posframe-show-params `(:internal-border-width 2
                                                                    :internal-border-color ,fg-main
                                                                    :poshandler posframe-poshandler-frame-bottom-center)))

      (with-eval-after-load 'meow
        (set-face-attribute 'meow-normal-indicator nil
                            :background (if (eq appearance 'light) blue blue-d)
                            :foreground white)
        (set-face-attribute 'meow-motion-indicator nil
                            :background (if (eq appearance 'light) violet violet-d)
                            :foreground white)
        (set-face-attribute 'meow-keypad-indicator nil
                            :background (if (eq appearance 'light) red red-d)
                            :foreground white)
        (set-face-attribute 'meow-insert-indicator nil
                            :background (if (eq appearance 'light) green green-d)
                            :foreground white)
        (set-face-attribute 'meow-beacon-indicator nil
                            :background (if (eq appearance 'light) yellow yellow-d)
                            :foreground white))

      t)))

(defun schrenker/apply-overlay (appearance)
  "Apply solarized theme overlay based on APPEARANCE."
  (setopt emacs-appearance appearance)
  (schrenker/solarized-theme-overlay appearance))

(defun schrenker/setup-theme ()
  "Apply theme based on environment. Apply default appearance if graphical system is not present, or if no other condition is met. If system is darwin, then use ns-appearance symbol."
  (cond
   ((not (display-graphic-p))(schrenker/apply-overlay emacs-appearance))
   ((eq system-type 'darwin)(progn
                              (add-hook 'ns-system-appearance-change-functions #'schrenker/apply-overlay)
                              (schrenker/apply-overlay ns-system-appearance)))
   (t (schrenker/apply-overlay emacs-appearance))))


(provide 'solarized-overlay)
;;; solarized-overlay.el ends here.

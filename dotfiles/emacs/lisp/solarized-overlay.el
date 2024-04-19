;;; solarized-overlay.el --- Personal customized theme -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This file contains my own modifications to solarized theme, including color handling for theme switching.

;;; Code:
(defvar emacs-appearance 'dark
  "This variable holds initial value for theme if there is no dynamic system in place (macos), or value of theme that has been switched to.")

(defun schrenker/solarized-theme-overlay (appearance)
    ;; Function that is there just to make my life easier. Reapplies all visual updates, and that's it.
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
        (green-2fg    (if (eq appearance 'light) "#5b6e35" "#97a35f")))
      (progn
        (setq-default org-todo-keyword-faces
                      `(("NEXT" :foreground ,yellow :weight bold :inverse-video t)
                        ("TODO" :foreground ,magenta :weight bold :inverse-video t)
                        ("INPROGRESS" :foreground ,green :weight bold :inverse-video t)
                        ("BLOCKED" :foreground ,orange :weight bold :inverse-video t)
                        ("ONHOLD" :foreground ,cyan :weight bold :inverse-video t)
                        ("REVIEW" :foreground ,blue :weight bold :inverse-video t)
                        ("DONE" :foreground ,fg-emph :weight bold :inverse-video t)
                        ("CANCELLED" :foreground ,fg-alt :weight bold :inverse-video t)
                        ("DELEGATED"  :foreground ,fg-main :weight bold :inverse-video t))
                      org-priority-faces
                      `((?A :foreground ,red :weight bold :inverse-video t)
                        (?B :foreground ,yellow :weight bold :inverse-video t)
                        (?C :foreground ,violet :weight bold :inverse-video t)
                        (?D :foreground ,fg-emph :weight bold :inverse-video t))
                      org-src-block-faces
                      `(("emacs-lisp" (:background ,magenta-1bg :extend t))
                        ("python" (:background ,green-1bg :extend t))
                        ("yaml" (:background ,cyan-1bg :extend t))
                        ("json" (:background ,blue-1bg :extend t))
                        ("bash" (:background ,green-1bg :extend t))
                        ("sh" (:background ,green-1bg :extend t))
                        ("shell" (:background ,green-1bg :extend t))
                        ("fish" (:background ,green-1bg :extend t))
                        ("nix" (:background ,blue-1bg :extend t)))
                      org-roam-ui-custom-theme
                      `((bg . ,(if (eq appearance 'light) "#FDF6E3" "#002b36"))
                        (bg-alt . ,(if (eq appearance 'light) "#EEE8D5" "#00212B"))
                        (fg . ,(if (eq appearance 'light) "#556b72" "#839496"))
                        (fg-alt . ,(if (eq appearance 'light) "#7B8787" "#657b83"))
                        (grey . ,(if (eq appearance 'light) "#E1DBCD" "#56697A"))
                        (blue . ,blue)
                        (cyan . ,cyan)
                        (dark-blue . ,blue-d)
                        (dark-cyan . ,cyan-d)
                        (green . ,green)
                        (magenta . ,magenta)
                        (orange . ,orange)
                        (red . ,red)
                        (teal . ,teal)
                        (violet . ,violet)
                        (yellow . ,yellow)
                        (base0 . ,(if (eq appearance 'light) "#FFFBF0" "#073642"))
                        (base1 . ,(if (eq appearance 'light) "#FCF8ED" "#03282F"))
                        (base2 . ,(if (eq appearance 'light) "#FCF7E8" "#00212C"))
                        (base3 . ,(if (eq appearance 'light) "#F2E6CE" "#13383C"))
                        (base4 . ,(if (eq appearance 'light) "#E1DBCD" "#56697A"))
                        (base5 . ,(if (eq appearance 'light) "#D6D6D6" "#405A61"))
                        (base6 . "#96A7A9")
                        (base7 . "#788484")
                        (base8 . "#626C6C"))
                      org-modern-todo-faces org-todo-keyword-faces
                      org-modern-priority-faces org-priority-faces)

        (mapc #'disable-theme custom-enabled-themes)
        (pcase appearance
          ('light (load-theme 'solarized-light t))
          ('dark (load-theme 'solarized-dark t)))

        (kind-icon-reset-cache)

        (prism-set-colors
          :num 20
          :desaturations '(0 5 10 15 20)
          :lightens '(0 -15 -30 -45 -60)
          :colors (list blue green cyan yellow))

        (set-face-background 'hl-line bg-alt)

        (set-face-attribute 'fringe nil :background bg-alt)
        (set-face-attribute 'line-number nil :background bg-alt)
        (set-face-attribute 'line-number-current-line nil :background bg-alt)
        (set-face-attribute 'diff-hl-change nil :background blue-2bg :foreground (if (eq appearance 'light) blue-d blue-l))
        (set-face-attribute 'diff-hl-insert nil :background green-2bg :foreground (if (eq appearance 'light) green-d green-l))
        (set-face-attribute 'diff-hl-delete nil :background red-2bg :foreground (if (eq appearance 'light) red-d red-l))

        (set-face-attribute 'mode-line nil :background blue-2bg :foreground fg-emph :overline t)

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
        (set-face-attribute 'link nil :foreground cyan :slant 'italic )
        (set-face-attribute 'org-checkbox nil :box `(:line-width (3 . 1) :color ,bg-alt) :background bg-alt)
        (set-face-attribute 'italic nil :slant 'italic :underline nil)

        (set-face-attribute 'org-modern-todo nil :height 1.0 :weight 'bold :box '(:line-width (1 . 0)))
        (set-face-attribute 'org-modern-date-active nil :foreground fg-emph :background bg-alt)
        (set-face-attribute 'org-modern-date-inactive nil :foreground fg-alt :background bg-alt)
        (set-face-attribute 'org-modern-time-active nil :foreground fg-emph :background bg-main :inverse-video t)
        (set-face-attribute 'org-modern-time-inactive nil :foreground fg-alt :background bg-main :inverse-video t)
        (custom-set-faces `(org-modern-tag ((t (:inherit (secondary-selection org-modern-label) :weight bold :foreground ,violet :inverse-video t)))))
        (custom-set-faces `(org-modern-statistics ((t (:inherit org-modern-label :weight bold :background ,bg-alt :foreground ,green)))))
        (advice-add
         'org-modern--update-label-face
         :override
         (lambda (&rest r)
           (set-face-attribute 'org-modern-label nil :height 1.0 :box nil)))

        (with-eval-after-load 'ace-window
          (custom-set-faces `(aw-leading-char-face ((t (:inherit org-modern-label :width expanded :weight bold :background ,magenta :foreground ,bg-main :height 3.0 ))))))

        (with-eval-after-load 'dirvish
          (custom-set-faces `(dired-header ((t (:weight bold :background "unspecified" :foreground ,blue)))))
          (set-face-foreground 'vc-edited-state yellow)
          (set-face-foreground 'vc-locally-added-state green)
          (set-face-foreground 'vc-removed-state red)
          (set-face-foreground 'vc-missing-state fg-alt)
          (set-face-foreground 'vc-conflict-state orange)
          (set-face-foreground 'vc-locked-state violet)
          (set-face-foreground 'vc-needs-update-state blue))

        (with-eval-after-load 'embark
          (set-face-background 'embark-target (if (eq appearance 'light) white black)))

        (run-with-idle-timer 0 nil (lambda ()(mapc (lambda (buffer) (with-current-buffer buffer (when (derived-mode-p 'org-mode)(font-lock-update)))) (buffer-list)))))))

(defun schrenker/apply-overlay (appearance)
    (setq emacs-appearance appearance)
    (schrenker/solarized-theme-overlay appearance)
    (ignore-errors (org-roam-ui-sync-theme)))

(defun schrenker/initial-apply-overlay ()
  (cond
   ((not (display-graphic-p))(schrenker/apply-overlay emacs-appearance))
   ((eq system-type 'darwin)(progn
                              (add-hook 'ns-system-appearance-change-functions #'schrenker/apply-overlay)
                              (schrenker/apply-overlay ns-system-appearance)))
   (t (schrenker/apply-overlay emacs-appearance))))


(provide 'solarized-overlay)
;;; solarized-overlay.el ends here.

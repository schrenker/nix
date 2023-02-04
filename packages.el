;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! doom-themes)

(package! ace-window :pin "77115afc1b0b9f633084cf7479c767988106c196")

(package! evil-commentary)
(package! evil-snipe :disable t)

(package! key-chord)

(package! org-kanban)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(unpin! org-roam)
(package! org-roam-ui)

(package! noflet)

(package! org-autolist)

(package! treemacs-all-the-icons)

(package! cheat-sh)

(package! inheritenv)

(package! ox-confluence :recipe (:host github :repo "nan0scho1ar/ox-confluence-modern" :files ("*.el")))

(package! kele)

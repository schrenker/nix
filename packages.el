;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

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

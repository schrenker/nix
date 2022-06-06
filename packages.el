;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! evil-commentary)
(package! evil-snipe :disable t)

(package! key-chord)

(package! org-kanban)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(unpin! org-roam)
(package! org-roam-ui)

(package! treemacs-all-the-icons)

(package! cheat-sh)

(package! nov.el)
(package! nov-xwidget :recipe (:host "github" :repo "chenyanming/nov-xwidget"))

(package! esxml)

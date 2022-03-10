;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! evil-commentary)
(package! evil-snipe :disable t)

(package! key-chord)

(package! org-kanban)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! treemacs-all-the-icons)

(package! restclient)
(package! ob-restclient)
(package! cheat-sh)
(package! ob-powershell :recipe (:host github :repo "MoisMoshev/ob-powershell"))

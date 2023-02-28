;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! doom-themes)

(package! ace-window :pin "77115afc1b0b9f633084cf7479c767988106c196")

(package! evil-commentary)
(package! evil-snipe :disable t)

(package! key-chord)

(package! org-kanban)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! polish-holidays :recipe (:host github :repo "mikolajb/emacs-polish-holidays"))
(package! german-holidays)

(unpin! org-roam)
(package! org-roam-ui)

(package! noflet)

(package! org-autolist)

(package! treemacs-all-the-icons)

(package! eat :recipe (:host codeberg
                       :repo "akib/emacs-eat"
                       :files ("*.el" ("term" "term/*.el") "*.texi"
                               "*.ti" ("terminfo/e" "terminfo/e/*")
                               ("terminfo/65" "terminfo/65/*")
                               ("integration" "integration/*")
                               (:exclude ".dir-locals.el" "*-tests.el"))))

(package! cheat-sh)

(package! inheritenv)

(package! x509-mode)

(package! ox-confluence :recipe (:host github :repo "nan0scho1ar/ox-confluence-modern" :files ("*.el")))

(package! kele)

(package! prism)

(package! vlf :recipe (:host github :repo "emacs-straight/vlf" :files ("*.el")) :pin "cacdb359f8c37c6e7e4c7937462b632d22462130")

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt
             mac-pass-control-to-system nil)))
(map! "M-c" 'kill-ring-save)
(map! "M-v" 'yank)
(map! "M-q" 'save-buffers-kill-terminal)
(map! :leader
      (:prefix-map ("k" . "kubernetes?")
       :desc "kubernetes-overview" "k" #'kubernetes-overview))

(require 'key-chord)
(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(require 'uniquify)

(setq-default
 tab-width 2
 window-combination-resize t
 truncate-string-ellipsis "…"
 uniquify-buffer-name-style 'forward
 frame-title-format '("%f [%m]"))

(setq

 user-full-name "Sebastian Zawadzki"
 user-mail-address (rot13 "mnjnqmxvf95@tznvy.pbz")

 doom-theme 'doom-one
 doom-font (font-spec :family "JetBrains Mono NL" :size 12 )
 doom-themes-treemacs-theme "doom-colors"
 display-line-numbers-type 'relative

 doom-modeline-icon (display-graphic-p)
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-buffer-state-icon t

 +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2)
 initial-frame-alist '((fullscreen . maximized))

 uniquify-buffer-name-style 'forward
 uniquify-separator "/"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"

 evil-vsplit-window-right t
 evil-split-window-below t

 auto-save-default t
 make-backup-files t

 auth-sources '("~/.authinfo.gpg")
 auth-source-cache-expiry nil

 org-directory "/Users/sebastian/code/engineer_notebook"
 org-babel-python-command "python3"
 org-superstar-headline-bullets-list '("⁖")
 org-superstar-prettify-item-bullets nil
 org-log-done 'time

 evil-want-Y-yank-to-eol nil

 projectile-auto-discover t
 projectile-project-search-path '("~/code")

 company-minimum-prefix-length 3
 company-auto-complete nil
 company-idle-delay 0
 company-require-match nil

 cider-repl-pop-to-buffer-on-connect nil
 cider-repl-display-in-current-window nil
 cider-repl-display-help-banner nil
 cider-repl-use-pretty-printing t

 TeX-engine-alist
   '((xetex "XeTeX -shell escape"
            "xetex -shell-escape"
            "xelatex -shell-escape"))

 scroll-margin 5)

(defun adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (setq doom-theme 'doom-one-light)
             (load-theme 'doom-one-light t))
    ('dark (setq doom-theme 'doom-one)
             (load-theme 'doom-one t))))

(add-hook! org-mode
                (adjust-org-company-backends)
                (electric-indent-local-mode -1))
(add-hook 'org-mode-hook(lambda () ( company-mode -1)))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(after! lsp-clients
        (lsp-register-client
        (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                        :major-modes '(python-mode)
                        :remote? t
                        :server-id 'pyls-remote)))

(after! org
  (setq
   org-insert-heading-respect-content nil
   org-todo-keywords '((sequence "[TODO](t)" "[INPROGRESS](i)" "[WAITING](w)"  "|" "[DONE](d)" "[CANCELLED](c)"))
   org-todo-keyword-faces
   '(("[TODO]" :foreground "#8741bb" :weight normal)
     ("[INPROGRESS]" :foreground "#98BE65" :weight normal)
     ("[WAITING]" :foreground "#DA8548" :weight normal)
     ("[DONE]" :foreground "#9FA4BB" :weight normal)
     ("[CANCELLED]" :foreground "#574C58" :weight normal)))
  (custom-set-faces!
    '(org-level-1 :height 1.04 :inherit outline-1)
    '(org-level-2 :height 1.04 :inherit outline-2)
    '(org-level-3 :height 1.03 :inherit outline-3)
    '(org-level-4 :height 1.03 :inherit outline-4)
    '(org-level-5 :height 1.02 :inherit outline-5)
    '(org-level-6 :height 1.02 :inherit outline-6)
    '(org-level-7 :height 1.01 :inherit outline-7)
    '(org-level-8 :height 1.01 :inherit outline-8)))

(after! org-fancy-priorities
  (setq
   org-priority-highest '?A
   org-priority-lowest  '?C
   org-priority-default '?C
   org-priority-start-cycle-with-default t
   org-priority-faces '((65 :foreground "#FF6C6B" :weight normal)
                        (66 :foreground "#ECBE7B" :weight normal)
                        (67 :foreground "#51AFEF" :weight normal))
   org-fancy-priorities-list '((65 . "⁂")
                               (66 . "⁑")
                               (67 . "⁕"))))
;; backspace choppiness
(after! tex-mode
  (map-delete sp-pairs 'LaTeX-mode)
  (map-delete sp-pairs 'latex-mode)
  (map-delete sp-pairs 'tex-mode)
  (map-delete sp-pairs 'plain-tex-mode))


(projectile-discover-projects-in-search-path)
(projectile-cleanup-known-projects)

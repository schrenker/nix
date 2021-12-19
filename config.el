;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt
             mac-pass-control-to-system nil)))

(setq-default
 tab-width 2
 frame-title-format '("%f [%m]"))

(setq
 user-full-name "Sebastian Zawadzki"
 user-mail-address (rot13 "mnjnqmxvf95@tznvy.pbz")

 auto-save-default t
 make-backup-files t

 +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2)
 initial-frame-alist '((top . 1) (left . 1) (width . 114) (height . 32))

 doom-theme 'doom-one
 doom-font (font-spec :family "JetBrains Mono NL" :size 13 )
 doom-themes-treemacs-theme "doom-colors"
 display-line-numbers-type 'relative
 doom-modeline-icon (display-graphic-p)
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-buffer-state-icon t

 org-directory "/Users/sebastian/Code/engineer_notebook"
 org-babel-python-command "python3"
 org-superstar-headline-bullets-list '("⁖")
 org-superstar-prettify-item-bullets nil

 evil-want-Y-yank-to-eol nil

 scroll-margin 7)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(require 'key-chord)
(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(defun adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
(add-hook! org-mode (adjust-org-company-backends))
(add-hook! org-mode (electric-indent-local-mode -1))
(add-hook 'org-mode-hook(lambda () ( company-mode -1)))

(after! org (setq org-insert-heading-respect-content nil))
(after! org
        (setq org-todo-keywords
                '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)"  "|" "DONE(d)" "CANCELLED(c)"))))
(after! lsp-clients
        (lsp-register-client
        (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                        :major-modes '(python-mode)
                        :remote? t
                        :server-id 'pyls-remote)))

(after! org-fancy-priorities
  (setq
   org-priority-highest '?A
   org-priority-lowest  '?C
   org-priority-default '?C
   org-priority-start-cycle-with-default t
   org-priority-faces '((65 :foreground "#D4213D")
                        (66 :foreground "#FADA5E")
                        (67 :foreground "#88AED0"))
   org-fancy-priorities-list '((65 . "HIGH")
                               (66 . "MID")
                               (67 . "LOW"))))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt
             mac-pass-control-to-system nil)))

(require 'key-chord)
(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(require 'uniquify)

(setq-default
 tab-width 2
 uniquify-buffer-name-style 'forward
 window-combination-resize t
 truncate-string-ellipsis "…"
 frame-title-format '("%f [%m]"))

(setq
 user-full-name "Sebastian Zawadzki"
 user-mail-address (rot13 "mnjnqmxvf95@tznvy.pbz")

 doom-theme 'doom-one
 doom-font (font-spec :family "JetBrains Mono NL" :size 13 )
 doom-big-font (font-spec :family "JetBrains Mono NL" :size 16)
 doom-themes-treemacs-theme "doom-colors"
 display-line-numbers-type 'relative

 doom-modeline-icon (display-graphic-p)
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-buffer-state-icon t

 +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2)
 initial-frame-alist '((fullscreen . maximized))

 uniquify-separator "/"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"

 evil-vsplit-window-right t
 evil-split-window-below t

 auto-save-default t
 make-backup-files t

 auth-sources '("~/.authinfo.gpg")
 auth-source-cache-expiry nil

 org-directory "/Users/sebastian/Code/engineer_notebook"
 org-babel-python-command "python3"
 org-superstar-headline-bullets-list '("⁖")
 org-superstar-prettify-item-bullets nil

 scroll-margin 5)

(setq!
 evil-want-Y-yank-to-eol nil)


(add-hook! org-mode
                (adjust-org-company-backends)
                (electric-indent-local-mode -1))
(add-hook 'org-mode-hook(lambda () ( company-mode -1)))

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
   '(("[TODO]" :foreground "#EFEFEF" :weight normal)
     ("[INPROGRESS]" :foreground "#98BE65" :weight normal)
     ("[WAITING]" :foreground "#DA8548" :weight normal)
     ("[DONE]" :foreground "#9FA4BB" :weight normal)
     ("[CANCELLED]" :foreground "#574C58" :weight normal))))

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

(defun adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))

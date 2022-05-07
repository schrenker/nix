;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq  user-full-name "Sebastian Zawadzki"
       user-mail-address (rot13 "fronfgvna@mnjnqmxv.grpu"))

(setq custom-file nil)

(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt)))

(map! "M-c" 'kill-ring-save)
(map! "M-v" 'yank)
(map! "M-q" 'save-buffers-kill-terminal)
(map! "M-m" 'suspend-frame)

(map! "A-<backspace>" 'doom/delete-backward-word)

(menu-bar-mode 0)
(tool-bar-mode 0)

(setq doom-theme 'doom-solarized-light)

(setq fancy-splash-image (concat doom-private-dir "banner.png"))

(defun my/apply-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (setq doom-theme 'doom-solarized-light)
             (load-theme 'doom-solarized-light t))
    ('dark (setq doom-theme 'doom-solarized-dark)
             (load-theme 'doom-solarized-dark t)))
  (org-roam-ui-sync-theme))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; (setq  doom-font (font-spec :family "FiraCode Nerd Font" :style "Retina" :size 12))
(setq  doom-font (font-spec :family "JetBrains Mono NL" :size 13))

(setq initial-frame-alist '((fullscreen . maximized)))

(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))

(setq-default
 frame-title-format '("Doom")
 ns-use-proxy-icon nil)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "red"
    :weight bold :height 1.5 ))

(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t)

(setq-default window-combination-resize t)

(setq-default truncate-string-ellipsis "…")

(require 'uniquify)

(setq-default
 uniquify-buffer-name-style 'forward)

(setq uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(setq auto-save-default t)

(setq make-backup-files t)

(setq-default tab-width 2)

(setq  display-line-numbers-type 'visual)

(setq scroll-margin 5)

(setq require-final-newline nil)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq evil-want-Y-yank-to-eol t)

(setq +evil-want-o/O-to-continue-comments nil)

(setq evil-escape-key-sequence nil)

(map! :map evil-window-map
      :g "w" 'ace-window
      :g "p" 'treemacs-select-window)

(require 'key-chord)

(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(setq org-startup-folded 'nofold)

(map! :map org-mode-map
      :localleader "$" 'org-decrypt-entry
      :localleader "a i" 'org-display-inline-images)

(after! org
  (map! :nv "gj" #'evil-next-visual-line
        :nv "gk" #'evil-previous-visual-line))

(setq org-directory "/Users/sebastian/Code/brain"
      org-roam-directory org-directory
      org-default-notes-file (concat org-directory "/!capture.org"))

(setq org-tags-exclude-from-inheritance '("crypt"
                                          "moc"
                                          "inbox"
                                          "evergreen"
                                          "wip"
                                          "unpolished"))

(require 'org-crypt)

(setq org-crypt-disable-auto-save t
      org-crypt-key "Sebastian Zawadzki")

(add-hook! org-mode (electric-indent-local-mode -1))

(add-hook 'org-mode-hook 'org-appear-mode)

(setq org-display-remote-inline-images t
      org-startup-with-inline-images t
      org-image-actual-width nil)

(setq org-log-done 'time)

(after! org
  (setq
   org-crypt-disable-auto-save t
   org-priority-highest '?A
   org-priority-lowest  '?C
   org-priority-default '?C
   org-priority-start-cycle-with-default t
   org-priority-faces '((?A :foreground "#FF6C6B" :weight normal)
                        (?B :foreground "#ECBE7B" :weight normal)
                        (?C :foreground "#51AFEF" :weight normal))
   org-todo-keywords '((sequence "[TODO](t)" "[INPROGRESS](i)" "[WAITING](w)"  "|" "[DONE](d)" "[CANCELLED](c)"))
   org-todo-keyword-faces
   '(("[TODO]" :foreground "#8741bb" :weight normal)
     ("[INPROGRESS]" :foreground "#98BE65" :weight normal)
     ("[WAITING]" :foreground "#DA8548" :weight normal)
     ("[DONE]" :foreground "#9FA4BB" :weight normal )
     ("[CANCELLED]" :foreground "#574C58" :weight normal))))

(setq company-global-modes '(not org-mode))
(add-hook 'org-mode-hook (lambda () ( company-mode -1)))

(map! :map doom-leader-notes-map
      :g "r t" 'org-roam-ui-sync-theme
      :g "r o" 'org-roam-ui-open)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+modified: \n#+filetags: :inbox:\n\n")
                                      :immediate-finish t)))

(after! org
  (setq time-stamp-active t
    time-stamp-start "#\\+modified: [ \t]*"
    time-stamp-end "$"
    time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
(add-hook 'before-save-hook 'time-stamp))

(setq org-superstar-headline-bullets-list '("⁖"))

(after! org
  (custom-set-faces!
    '(org-level-1 :height 1.04 :inherit outline-1)
    '(org-level-2 :height 1.04 :inherit outline-2)
    '(org-level-3 :height 1.04 :inherit outline-3)
    '(org-level-4 :height 1.04 :inherit outline-4)
    '(org-level-5 :height 1.04 :inherit outline-5)
    '(org-level-6 :height 1.04 :inherit outline-6)
    '(org-level-7 :height 1.04 :inherit outline-7)
    '(org-level-8 :height 1.04 :inherit outline-8)))

(setq org-superstar-prettify-item-bullets nil)

;; (font-lock-add-keywords 'org-mode
;;  '(("^ *\\([-]\\) "
;;  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))

(add-hook 'org-mode-hook (lambda ()
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[-]" . "" ) prettify-symbols-alist)
  (push '("[X]" . "" ) prettify-symbols-alist)
  (push '("[#A]" . "⁂" ) prettify-symbols-alist)
  (push '("[#B]" . "⁑" ) prettify-symbols-alist)
  (push '("[#C]" . "⁕" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

(after! org-fancy-priorities
  (setq
   org-fancy-priorities-list '((65 . "⁂")
                               (66 . "⁑")
                               (67 . "⁕"))))

(setq org-tags-column -77)

(add-hook 'org-mode-hook #'+word-wrap-mode)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-hide-emphasis-markers t)

(setq +treemacs-git-mode 'deferred)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(setq  doom-themes-treemacs-theme "doom-colors")

(setq company-auto-complete nil)

(setq company-tooltip-align-annotations t
      company-tooltip-minimum (- scroll-margin 1)
      company-tooltip-flip-when-above t)

(setq company-minimum-prefix-length 1
      company-require-match nil)

(setq company-idle-delay 0)

(after! flyspell
  (setq flyspell-lazy-idle-seconds 2))

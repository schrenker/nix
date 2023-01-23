;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (require 'profiler)
;; (profiler-start 'cpu)

(setq  user-full-name "Sebastian Zawadzki"
       user-mail-address (rot13 "fronfgvna@mnjnqmxv.grpu"))

(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt)))

(map! "M-c" #'kill-ring-save)
(map! "M-v" #'yank)
(map! "M-q" #'save-buffers-kill-terminal)
(map! "M-m" #'suspend-frame)
(map! "M-w" #'kill-this-buffer)

(map! "A-<backspace>" #'doom/delete-backward-word)

(setq +workspaces-on-switch-project-behavior 'non-empty)

(add-hook! 'doom-first-buffer-hook
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(which-function-mode)

(setq doom-theme 'doom-solarized-light)

(defun my/apply-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (setq doom-theme 'doom-solarized-light)
             (load-theme 'doom-solarized-light t))
    ('dark (setq doom-theme 'doom-solarized-dark)
             (load-theme 'doom-solarized-dark t)))
  (org-roam-ui-sync-theme))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(unless (display-graphic-p)
  (solaire-global-mode -1))

(setq doom-font (font-spec :family "JetBrains Mono NL" :size 13)
      doom-big-font (font-spec :family "JetBrains Mono NL" :size 26)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

(setq doom-themes-treemacs-enable-variable-pitch nil)

(setq fancy-splash-image "~/.config/doom/banner.png")

(setq initial-frame-alist '((fullscreen . maximized)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(setq-default
 frame-title-format '("Doom")
 ns-use-proxy-icon nil)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "red"
    :weight bold
    :height 2.5))
(ace-window-posframe-mode 1)

(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))

(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-icon nil
      doom-modeline-buffer-state-icon t)

(setq-default window-combination-resize t)

(setq-default truncate-string-ellipsis "…")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers;       uniquify-ignore-buffers-re "^\\*")
(after! persp-mode
  (setq-hook! 'persp-mode-hook uniquify-buffer-name-style 'forward))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(require 'centaur-tabs)
(centaur-tabs-group-by-projectile-project)

(setq centaur-tabs-show-count t)

(add-hook 'ibuffer-mode-hook 'centaur-tabs-local-mode)
(add-hook 'org-agenda-mode-hook 'centaur-tabs-local-mode)

(setq centaur-tabs-gray-out-icons 'buffer)

(setq auto-save-default t)

(setq make-backup-files t)

(setq-default tab-width 4)

(setq display-line-numbers-type 'visual)

(setq scroll-margin 5)

(setq require-final-newline nil)

(setq evil-want-fine-undo t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq evil-want-Y-yank-to-eol t)

(setq +evil-want-o/O-to-continue-comments nil)

(defun schrenker/evil-change (orig-fn beg end &optional type _ &rest args)
    (apply orig-fn beg end type ?_ args))
(advice-add 'evil-change :around 'schrenker/evil-change)

(setq evil-kill-on-visual-paste nil)

(setq evil-escape-key-sequence nil)

(map! :map evil-window-map
      :g "w" #'ace-window
      :g "p" #'treemacs-select-window)

(require 'key-chord)

(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(setq org-startup-folded 'nofold)

(after! org
  (add-hook 'before-save-hook 'org-update-all-dblocks))

(map! :map org-mode-map
      :localleader "$" #'org-decrypt-entry
      :localleader "a i" #'org-display-inline-images)

(after! org
  (map! :map org-mode-map
        :nv "gj" #'evil-next-visual-line
        :nv "gk" #'evil-previous-visual-line))

(setq org-directory "/Users/sebastian/Library/Mobile Documents/com~apple~CloudDocs/brain"
      org-roam-directory org-directory
      org-archive-location "archive/%s_archive::"
      org-default-notes-file (concat org-directory "/20221222131538-personal.org")
      +org-capture-notes-file org-default-notes-file)

(setq org-tags-exclude-from-inheritance '("crypt"
                                          "moc"
                                          "inbox"))

(require 'org-crypt)

(setq org-crypt-disable-auto-save t
      org-crypt-key (rot13 "fronfgvna@mnjnqmxv.grpu"))

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
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "ONHOLD(o)" "REVIEW(r)" "|" "DONE(d)" "DELEGATED(e)" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("TODO" :foreground "#8741bb" :weight bold :inverse-video t)
     ("INPROGRESS" :foreground "#98BE65" :weight bold :inverse-video t)
     ("BLOCKED" :foreground "#DA8548" :weight bold :inverse-video t)
     ("ONHOLD" :foreground "#2AA198" :weight bold :inverse-video t)
     ("REVIEW" :foreground "#00BFFF" :weight bold :inverse-video t)
     ("DONE" :foreground "#9FA4BB" :weight bold :inverse-video t )
     ("CANCELLED" :foreground "#574C58" :weight bold :inverse-video t)
     ("DELEGATED"  :foreground "#6c71c4" :weight bold :inverse-video t))))

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

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◆"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◇"))))))

;; (add-hook 'org-mode-hook (lambda ()
;;   (push '("[#A]" . "⁂" ) prettify-symbols-alist)
;;   (push '("[#B]" . "⁑" ) prettify-symbols-alist)
;;   (push '("[#C]" . "⁕" ) prettify-symbols-alist)
;;   (prettify-symbols-mode)))

(after! org-fancy-priorities
  (setq
   org-fancy-priorities-list '((65 . "⁂")
                               (66 . "⁑")
                               (67 . "⁕"))))

(setq org-tags-column -77)

(add-hook 'org-mode-hook #'+word-wrap-mode)

(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-hide-emphasis-markers t)

(map! :map doom-leader-notes-map
      :g "r t" #'org-roam-ui-sync-theme
      :g "r o" #'org-roam-ui-open)

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
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: showeverything\n#+date: %U\n#+modified: \n#+filetags: :inbox:\n\n")
                                      :immediate-finish t)))

(after! org
  (setq time-stamp-active t
    time-stamp-start "#\\+modified: [ \t]*"
    time-stamp-end "$"
    time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
(add-hook 'before-save-hook 'time-stamp))

(after! org
  (setq org-capture-templates
        '(
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "** %U\n%i%?" :empty-lines 1)
          ("t" "Task" entry (file+headline org-default-notes-file "Tasks" "Backlog")
           "** TODO %?" :empty-lines 1)
          )))

(require 'noflet)
(defun schrenker/make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (top . 300)
                (left . 700)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

(setq company-global-modes '(not org-mode))
(add-hook 'org-mode-hook (lambda () ( company-mode -1)))

(setq +treemacs-git-mode 'deferred)

(setq treemacs-follow-mode t)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(setq doom-themes-treemacs-theme "doom-colors")

(setq corfu-preview-current 'insert
      corfu-preselect 'prompt ;; Disable candidate preselection
      corfu-excluded-modes
      '(erc-mode
        circe-mode
        help-mode
        gud-mode
        vterm-mode))
        ;; org-mode))

(map! ;;:desc "complete" "TAB" #'completion-at-point
 :map 'corfu-map
 :desc "next" "TAB" #'corfu-next
 :desc "next" "<tab>" #'corfu-next
 :desc "next" [tab] #'corfu-next
 :desc "previous" "S-TAB" #'corfu-previous
 :desc "previous" "<backtab>"  #'corfu-previous
 :desc "previous" [backtab] #'corfu-previous)

(global-corfu-mode)

(setq +lsp-company-backends nil
      +vertico-company-completion-styles nil)

(setq vterm-always-compile-module t)

(setq vterm-max-scrollback 100000)
      ;; vterm-buffer-name-string "VT: %s")

(map! :after vterm
       :map vterm-mode-map
       :ni "<tab>" #'vterm-send-tab
       :nvi "M-v" #'evil-collection-vterm-paste-after
       :nvi "M-c" #'evil-yank
       :i   "A-<backspace>" '(lambda () (interactive) (vterm-send-key (kbd "C-w"))))

(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)

(after! flyspell
  (setq flyspell-lazy-idle-seconds 2))

(require 'inheritenv)
(inheritenv-add-advice #'with-temp-buffer)

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

(unless IS-MAC
  ;;Start emacs non-maximized
  (setq initial-frame-alist '((top . 1) (left . 1) (width . 120) (height . 40)))
  ;;Unset problematic keybinds
  (map! "M-m" nil))

(load "~/.config/doom/work.el" t t)

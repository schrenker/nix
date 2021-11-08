;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sebastian Zawadzki"
      user-mail-address "zawadzkis95@gmail.com")
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrains Mono NL" :size 13 ))
(setq display-line-numbers-type 'relative)
(setq doom-themes-treemacs-theme "doom-colors")

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(setq-default tab-width 2)
(setq-default frame-title-format '("%f [%m]"))

;; Uniquify buffer names in buffer tab
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq scroll-margin 7)

(require 'key-chord)
(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

(setq! evil-want-Y-yank-to-eol nil)

(setq org-directory "/Users/sebastian/Code/engineer_notebook")
;;(setq org-roam-directory "/Users/sebastian/Code/engineer_notebook")

;;(use-package! websocket
;;    :after org-roam)

;;(use-package! org-roam-ui
;;    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;    :hook (after-init . org-roam-ui-mode)
;;    :config
;;    (setq org-roam-ui-sync-theme t
;;          org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;          org-roam-ui-open-on-start t))

;; Remap for window switching, do it doesn't require `w` key
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon (display-graphic-p))
;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)
;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)
;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

(setq org-babel-python-command "python3")

;; Prevents freezes on "look"
(add-hook 'org-mode-hook(lambda () ( company-mode -1)))

(after! org-fancy-priorities
  (setq
   org-priority-highest '?A
   org-priority-lowest  '?D
   org-priority-default '?D
   org-priority-start-cycle-with-default t
   org-priority-faces '((65 :foreground "#F25022")
                        (66 :foreground "#00A4EF")
                        (67 :foreground "#FFB900")
                        (68 :foreground "#737373"))
   org-fancy-priorities-list '((65 . "UI")
                               (66 . "NI")
                               (67 . "UN")
                               (68 . "NN"))))

(after! org
    (setq org-todo-keywords
          '((sequence "TODO" "DOING" "|" "DONE"))))

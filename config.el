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

;; Eisenhower Matrix
;; UI: Urgent Important
;; NI: Not urgent but Important
;; UN: Urgent but Not important
;; NN: Not urgent and Not important
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

;; TODO: base for all tasks
;; NEXT: planned to be done in near future
;; DOING: currently in progress
;; WAITING: Tasks, that were in progress, but the progress halted
(after! org
    (setq org-todo-keywords
          '((sequence "TODO" "NEXT" "DOING" "WAITING"  "|" "DONE" "WONTDO"))))


(lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(require 'key-chord)
(require 'treemacs-all-the-icons)
(require 'uniquify)
(require 'org-crypt)

(cond (IS-MAC
       (setq mac-command-modifier       'meta
             mac-option-modifier        'alt)))

(map! "M-c" 'kill-ring-save)
(map! "M-v" 'yank)
(map! "M-q" 'save-buffers-kill-terminal)
;; (map! :leader
;;       (:prefix-map ("k" . "kubernetes?")
;;        :desc "kubernetes-overview" "k" #'kubernetes-overview))

(map! :map evil-window-map
      :g "w" 'ace-window
      :g "t" 'treemacs-select-window)

(key-chord-define evil-insert-state-map ";;" 'right-char)
(key-chord-mode 1)

;; (set-popup-rule! "^\\*cider-repl*" :actions '(display-buffer-in-side-window) :side 'right :width 100 :quit nil :select nil)

(setq-default
 tab-width 2
 window-combination-resize t
 truncate-string-ellipsis "…"
 uniquify-buffer-name-style 'forward
 ns-use-proxy-icon nil
 frame-title-format '("Doom"))

(setq
 user-full-name "Sebastian Zawadzki"
 user-mail-address (rot13 "mnjnqmxvf95@tznvy.pbz")

 initial-frame-alist '((fullscreen . maximized))
 doom-theme 'doom-one
 doom-font (font-spec :family "JetBrains Mono NL" :size 12 )
 doom-themes-treemacs-theme "doom-colors"
 display-line-numbers-type 'relative
 scroll-margin 5

 doom-modeline-icon (display-graphic-p)
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-buffer-state-icon t

 +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2)

 uniquify-buffer-name-style 'forward
 uniquify-separator "/"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"

 evil-vsplit-window-right t
 evil-split-window-below t
 evil-want-Y-yank-to-eol nil
 +evil-want-o/O-to-continue-comments nil

 auto-save-default t
 make-backup-files t
 require-final-newline nil

 auth-sources '("~/.authinfo.gpg")
 auth-source-cache-expiry nil

 org-directory "/Users/sebastian/code/engineer_notebook"
 org-default-notes-file (concat org-directory "/!capture.org")
 org-babel-python-command "python3"
 org-superstar-headline-bullets-list '("⁖")
 org-superstar-prettify-item-bullets nil
 org-log-done 'time
 org-tags-column -77
 org-tags-exclude-from-inheritance '("crypt")
 org-crypt-key "Sebastian Zawadzki"

 projectile-project-search-path '("~/code")

 company-tooltip-align-annotations t
 company-tooltip-minimum (- scroll-margin 1)
 company-tooltip-flip-when-above t
 company-minimum-prefix-length 1
 company-auto-complete nil
 company-idle-delay 0
 company-require-match nil

 flycheck-global-modes '(not LaTeX-mode latex-mode)

 TeX-engine-alist
   '((xetex "XeTeX -shell escape"
            "xetex -shell-escape"
            "xelatex -shell-escape")))

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

(defun eshell/cdd () (interactive) (eshell/cd-to-project))

(add-hook! org-mode
                (adjust-org-company-backends)
                (electric-indent-local-mode -1))

(add-hook 'org-mode-hook(lambda () ( company-mode -1)))

(add-hook 'org-mode-hook (lambda ()
  "Beautify Org Checkbox Symbol"
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(after! org
  (setq
   org-crypt-disable-auto-save t
   org-priority-highest '?A
   org-priority-lowest  '?C
   org-priority-default '?C
   org-priority-start-cycle-with-default t
   org-priority-faces '((65 :foreground "#FF6C6B" :weight normal)
                        (66 :foreground "#ECBE7B" :weight normal)
                        (67 :foreground "#51AFEF" :weight normal))
   org-todo-keywords '((sequence "[TODO](t)" "[INPROGRESS](i)" "[WAITING](w)"  "|" "[DONE](d)" "[CANCELLED](c)"))
   org-todo-keyword-faces
   '(("[TODO]" :foreground "#8741bb" :weight normal)
     ("[INPROGRESS]" :foreground "#98BE65" :weight normal)
     ("[WAITING]" :foreground "#DA8548" :weight normal)
     ("[DONE]" :foreground "#9FA4BB" :weight normal )
     ("[CANCELLED]" :foreground "#574C58" :weight normal)))
  (custom-set-faces!
    '(org-level-1 :height 1.05 :inherit outline-1)
    '(org-level-2 :height 1.05 :inherit outline-2)
    '(org-level-3 :height 1.04 :inherit outline-3)
    '(org-level-4 :height 1.04 :inherit outline-4)
    '(org-level-5 :height 1.03 :inherit outline-5)
    '(org-level-6 :height 1.03 :inherit outline-6)
    '(org-level-7 :height 1.02 :inherit outline-7)
    '(org-level-8 :height 1.02 :inherit outline-8)))

(after! org-fancy-priorities
  (setq
   org-fancy-priorities-list '((65 . "⁂")
                               (66 . "⁑")
                               (67 . "⁕"))))

(treemacs-load-theme "all-the-icons")

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "red"
    :weight bold :height 1.5 ))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
 '(("^ *\\([-]\\) "
 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


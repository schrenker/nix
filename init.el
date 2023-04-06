;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This is my personal configuration file I use at three different environment systems - on macOS, WSL2 (wslg) and msys2.

;;; Code:

(setq gc-cons-threshold (* 1024 1024 200))

(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq custom-file "/dev/null"
      backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil
      inhibit-startup-screen t
      load-prefer-newer t
      visible-bell (eq system-type 'gnu/linux)
      display-line-numbers-type 'visual
      scroll-margin 7
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      sentence-end-double-space nil
      auto-window-vscroll nil
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      native-comp-async-report-warnings-errors nil
      require-final-newline t
      mac-command-modifier 'meta
      mac-option-modifier 'alt
      mac-right-option-modifier nil
      user-full-name "Sebastian Zawadzki"
      user-mail-address (rot13 "fronfgvna@mnjnqmxv.grpu")
      initial-frame-alist '((fullscreen . maximized)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(savehist-mode 1)
(which-function-mode 1)
(winner-mode 1)
(pixel-scroll-mode 1)
(global-display-line-numbers-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "<A-backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-_") 'undo-redo)

(unbind-key (kbd "M-v"))
(unbind-key (kbd "M-r"))
(unbind-key (kbd "<f2>"))
(unbind-key (kbd "<f10>"))

(defalias 'yes-or-no-p 'y-or-n-p)

(if (eq system-type 'gnu/linux)
    (set-frame-font "JetBrains Mono 10" nil t)
  (set-frame-font "JetBrains Mono 13" nil t))

(defmacro schrenker/call-negative (form)
  "Macro for calling any command with negative argument. FORM in this case is function you want called."
  `(let ((current-prefix-arg -1))
     (call-interactively ,form)))

(use-package meow
  :init
  (meow-global-mode 1)
  :config
  (defun schrenker/meow-append-to-end-of-line ()
    "Go to the end of the line and enter insert mode."
    (interactive)
    (call-interactively #'meow-line)
    (call-interactively #'meow-append))

  (defun schrenker/meow-insert-at-beginning-of-line ()
    "Go to the beginnig of the line and enter insert mode."
    (interactive)
    (call-interactively #'meow-join)
    (call-interactively #'meow-append))

  (defun schrenker/meow-join-below ()
    "Join line below to current line"
    (interactive)
    (call-interactively #'meow-next)
    (call-interactively #'meow-join)
    (call-interactively #'meow-kill))

  (defun schrenker/meow-smart-append ()
    (interactive)
    (if (eolp)
        (call-interactively #'meow-insert)
      (call-interactively #'meow-append)))

  (defun schrenker/meow-find-backwards ()
    (interactive)
    (schrenker/call-negative 'meow-find))

  (defun schrenker/meow-till-backwards ()
    (interactive)
    (schrenker/call-negative 'meow-till))

  (when (eq system-type 'gnu/linux)

    (defun schrenker/wsl-copy-region-to-clipboard (start end)
      "Copy region to Windows clipboard."
      (interactive "r")
      (call-process-region start end "clip.exe" nil 0)
      (call-interactively #'meow-cancel-selection))

    (defun schrenker/wsl-clipboard-to-string ()
      "Return Windows clipboard as string."
      (let ((coding-system-for-read 'dos))
        (substring				; remove added trailing \n
         (shell-command-to-string "powershell.exe -Command Get-Clipboard") 0 -1)))

    (defun schrenker/wsl-paste-from-clipboard (arg)
      "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
      (interactive "P")
      (let ((clip (schrenker/wsl-clipboard-to-string)))
        (insert clip)
        (if arg (kill-new clip))))

    (global-set-key (kbd "M-w") 'schrenker/wsl-copy-region-to-clipboard)
    (global-set-key (kbd "C-y") 'schrenker/wsl-paste-from-clipboard))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-motion-overwrite-define-key)
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore)
   '("SPC" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '("#" . universal-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("'" . repeat)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . schrenker/meow-smart-append)
   '("A" . schrenker/meow-append-to-end-of-line)
   '("o" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . ignore)
   '("f" . meow-find)
   '("F" . schrenker/meow-find-backwards)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("O" . meow-open-above)
   '("I" . schrenker/meow-insert-at-beginning-of-line)
   '("i" . meow-insert)
   '("j" . meow-join)
   '("J" . schrenker/meow-join-below)
   '("k" . meow-kill)
   '("K" . helpful-at-point)
   '("l" . meow-till)
   '("L" . schrenker/meow-till-backwards)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("%" . meow-block)
   '("O" . meow-open-above)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . ignore)
   '("Q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("/" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("<escape>" . meow-cancel-selection)
   '("SPC" . ignore)) ; I don't need keypad

  (add-to-list 'meow-mode-state-list '(elpaca-ui-mode . motion))
  (add-to-list 'meow-mode-state-list '(dired-mode . motion))
  (add-to-list 'meow-mode-state-list '(ibuffer-mode . motion))

  (add-hook 'meow-insert-exit-hook 'corfu-quit)

  (setq meow-use-clipboard t
        meow-use-cursor-position-hack t
        meow-expand-exclude-mode-list nil
        meow-use-enhanced-selection-effect t
        meow-char-thing-table '((?r . round)
                                (?s . square)
                                (?c . curly)
                                (?S . string)
                                (?o . symbol)
                                (?w . window)
                                (?b . buffer)
                                (?p . paragraph)
                                (?l . line)
                                (?d . defun)
                                (?. . sentence))))

(use-package hydra
  :config
  (defhydra hydra-window (global-map "C-c w")
    "Window"
    ("u" winner-undo "undo")
    ("r" winner-redo "redo"))
  (defhydra hydra-text-scale (global-map "<f2>")
    "Text Zoom"
    ("=" text-scale-increase "Zoom In")
    ("-" text-scale-decrease "Zoom Out")))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 3)

  ;; Show more candidates
  (setq vertico-count 15)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle nil))

;; ;; A few more useful configurations...
(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-prefixes
                                    orderless-initialism)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :demand t
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("[tab]" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("[backtab]" . corfu-previous)
              ("S-SPC" . corfu-insert-separator)
              ("C-S-n" . corfu-move-to-minibuffer))

  :init
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 3
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (setq corfu-cycle t
        corfu-separator ?\s
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-preview-current 'insert
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        tab-always-indent 'complete)
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  (global-corfu-mode)
  (require 'corfu-history)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (require 'corfu-popupinfo)
  (setq corfu-popupinfo-delay '(1.0 . 1.0))
  (corfu-popupinfo-mode 1))

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :commands
  (completion-at-point ;; capf
   complete-tag       ;; etags
   cape-dabbrev       ;; or dabbrev-completion
   cape-history
   cape-file
   cape-keyword
   cape-symbol
   cape-abbrev
   cape-ispell
   cape-dict
   cape-sgml
   cape-rfc1345)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package kind-icon
  :commands (kind-icon-margin-formatter kind-icon-reset-cache)
  :init
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2))

(use-package dabbrev
  :after corfu
  :elpaca nil
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
        read-extended-command-predicate #'command-completion-default-include-p))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :bind ("C-c g g" . magit))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package solarized-theme
  :init
  (setq solarized-use-more-italic t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  :config
  (if (eq system-type 'darwin)
      (progn
        (defun schrenker/apply-theme (appearance)
          (mapc #'disable-theme custom-enabled-themes)
          (pcase appearance
            ('light (load-theme 'solarized-light t))
            ('dark (load-theme 'solarized-dark t)))
          (kind-icon-reset-cache))
        (add-hook 'ns-system-appearance-change-functions #'schrenker/apply-theme)
        (schrenker/apply-theme ns-system-appearance))
    (load-theme 'solarized-light t)))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package all-the-icons)

(use-package all-the-icons-ibuffer
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :after all-the-icons
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode 1))


(setq frame-title-format '(:eval (concat user-login-name "@" system-name (if buffer-file-truename " :: %f" " :|: [%b]")))
      ns-use-proxy-icon (display-graphic-p))

(use-package eat
  :elpaca (eat
           :host "codeberg.org"
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq eat-term-name "xterm-256color")
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package inheritenv
  :config
  (inheritenv-add-advice #'with-temp-buffer))

;; Example configuration for Consult
(use-package consult
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-r l" . consult-register-load)
         ("M-r s" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("M-r r" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  (setq consult-project-function   #'consult--default-project-function))

(use-package embark
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; ;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package posframe)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (require 'posframe)
  (setq aw-keys '(?e ?t ?u ?h ?o ?n ?a ?s)
        aw-dispatch-alist'((?k aw-delete-window "Delete Window")
                           (?m aw-swap-window "Swap Windows")
                           (?M aw-move-window "Move Window")
                           (?x aw-copy-window "Copy Window")
                           (?b aw-switch-buffer-in-window "Select Buffer")
                           (?\M-o aw-flip-window)
                           (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
                           (?w aw-split-window-fair "Split Fair Window")
                           (?v aw-split-window-vert "Split Vert Window")
                           (?z aw-split-window-horz "Split Horz Window")
                           (?K delete-other-windows "Delete Other Windows")
                           (?? aw-show-dispatch-help)))
  (custom-set-faces '(aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.5)))))
  (ace-window-posframe-mode 1))

(use-package perject
  :demand t
  :after savehist
  :config
  (defun schrenker/perject-switch-project-global ()
    (interactive)
    (let ((current-prefix-arg '(4))) ;; emulate C-u
      (call-interactively 'perject-switch)))

  (defun schrenker/perject-switch-collection ()
    (interactive)
    (schrenker/call-negative 'perject-switch))

  (add-to-list 'savehist-additional-variables 'perject--previous-collections)
  ;; Make perject load the collections that were previously open.
  ;; This requires configuring `savehist' (see next code block).
  (setq perject-load-at-startup nil
        perject-save-frames nil
        perject-frame-title-format nil
        perject-switch-to-new-collection t
        perject-save-on-exit 'all
        perject-reload-default '(keep t)
        perject-close-default '(t nil t)
        perject-delete-default '(nil t nil t))

  (perject-mode 1)
  :bind
  (:map perject-mode-map
        ("C-<tab> C-<tab> c" . perject-create)
        ("C-<tab> C-<tab> r" . perject-rename)
        ("C-<tab> C-<tab> R" . perject-rename-collection)
        ("C-<tab> C-<tab> f" . perject-create-new-frame)
        ("C-<tab> C-<tab> K" . perject-delete)
        ("C-<tab> C-<tab> E" . perject-open-close-or-reload)
        ("C-<tab> C-<tab> s" . perject-sort)
        ("C-<tab> C-<tab> n" . perject-next-project)
        ("C-<tab> C-<tab> p" . perject-previous-project)
        ("C-<tab> C-<tab> N" . perject-next-collection)
        ("C-<tab> C-<tab> P" . perject-previous-collection)
        ("C-<tab> C-<tab> TAB" . schrenker/perject-switch-project-global)
        ("C-<tab> C-<tab> C-<tab>" . schrenker/perject-switch-collection)
        ("C-<tab> C-<tab> a" . perject-add-buffer-to-project)
        ("C-<tab> C-<tab> d" . perject-remove-buffer-from-project)
        ("C-<tab> C-<tab> w" . perject-save)))

(use-package perject-consult
  :elpaca
  (perject-consult
   :host "github.com"
   :repo "overideal/perject"
   :main "perject-consult.el")
  :after (perject consult)
  :config
  ;; Hide the list of all buffers by default and set narrowing to all buffers to space.
  (consult-customize consult--source-buffer :hidden t :narrow 32)
  (consult-customize consult--source-hidden-buffer :narrow ?h)
  (add-to-list 'consult-buffer-sources 'perject-consult--source-collection-buffer)
  (add-to-list 'consult-buffer-sources 'perject-consult--source-project-buffer))

(use-package perject-ibuffer
  :elpaca
  (perject-ibuffer
   :host "github.com"
   :repo "overideal/perject"
   :main "perject-ibuffer.el")
  :after perject
  :init
  ;; By default restrict ibuffer to the buffers of the current project.
  (add-hook 'ibuffer-hook #'perject-ibuffer-enable-filter-by-project)
  :bind
  (:map ibuffer-mode-map
        ("a" . perject-ibuffer-add-to-project)
        ("K" . perject-ibuffer-remove-from-project)
        ("SPC" . perject-ibuffer-print-buffer-projects)
        ("/ y" . ibuffer-filter-by-collection)
        ("/ u" . ibuffer-filter-by-project)))

(use-package perject-tab
  :elpaca
  (perject-tab
   :host "github.com"
   :repo "overideal/perject"
   :main "perject-tab.el")
  :after perject
  :init
  (perject-tab-mode 1)
  :bind
  (:map perject-tab-mode-map
        ("C-<tab> o" . perject-tab-recent)
        ("C-<tab> p" . perject-tab-previous)
        ("C-<tab> n" . perject-tab-next)
        ("C-<tab> S" . perject-tab-set)
        ("C-<tab> s" . perject-tab-cycle-state)
        ("C-<tab> t" . perject-tab-create)
        ("C-<tab> T" . perject-tab-delete)
        ("C-<tab> r" . perject-tab-reset)
        ("C-<tab> i" . perject-tab-increment-index)
        ("C-<tab> I" . perject-tab-decrement-index)))

(use-package org
  :elpaca nil
  :init
  (setq time-stamp-active t
        time-stamp-start "#\\+modified: [ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp)
  :config
  (require 'org-crypt)
  (require 'org-agenda)
  (require 'org-capture)
  (load-file (concat user-emacs-directory "org-format.el"))
  (setq
   org-crypt-disable-auto-save t
   org-log-into-drawer "LOGBOOK"
   org-log-done 'time
   org-tags-exclude-from-inheritance '("crypt"
                                       "moc"
                                       "inbox")
   org-crypt-disable-auto-save t
   org-crypt-key (rot13 "fronfgvna@mnjnqmxv.grpu")
   org-tags-column -77
   org-directory "~/org"
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "a."))
   org-roam-directory org-directory
   org-archive-location "archive/%s_archive::"
   org-default-notes-file (concat org-directory "/20221222131538-personal.org")
   org-crypt-disable-auto-save t
   org-crypt-key (rot13 "fronfgvna@mnjnqmxv.grpu")
   org-priority-highest '?A
   org-priority-lowest  '?C
   org-priority-default '?C
   org-agenda-files '("~/org/")
   org-hide-emphasis-markers t
   org-priority-start-cycle-with-default t
   org-priority-faces '((?A :foreground "#DC322F" :weight normal)
                        (?B :foreground "#B58900" :weight normal)
                        (?C :foreground "#6C71C4" :weight normal))
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "BLOCKED(b@/!)" "ONHOLD(o@/!)" "REVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(e@/@)" "CANCELLED(c@/@)"))
   org-todo-keyword-faces
   '(("TODO" :foreground "#D33682" :weight bold :inverse-video t)
     ("INPROGRESS" :foreground "#859900" :weight bold :inverse-video t)
     ("BLOCKED" :foreground "#CB4B16" :weight bold :inverse-video t)
     ("ONHOLD" :foreground "#2AA198" :weight bold :inverse-video t)
     ("REVIEW" :foreground "#268BD2" :weight bold :inverse-video t)
     ("DONE" :foreground "#9FA4BB" :weight bold :inverse-video t )
     ("CANCELLED" :foreground "#574C58" :weight bold :inverse-video t)
     ("DELEGATED"  :foreground "#6c71c4" :weight bold :inverse-video t))
   org-capture-templates
   '(("p" "Personal Note" entry (file+headline org-default-notes-file "Notes") "** %U\n%i%?" :empty-lines 1)
     ("P" "Personal Task" entry (file+olp org-default-notes-file "Tasks" "Backlog") "** TODO %?\n%U" :empty-lines 1)))
  (add-hook 'org-mode-hook (visual-line-mode 1))
  (add-hook 'org-mode-hook #'org-format-on-save-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook (lambda () (save-excursion (when (org-find-dblock "kanban") (org-update-dblock)))) nil t))))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-roam
  :after org
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+startup: showeverything\n#+date: %U\n#+modified: \n#+filetags: :inbox:\n\n")
                                      :immediate-finish t))
        org-roam-directory (file-truename "~/org"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-kanban)

(use-package org-appear
  :elpaca (org-appear
           :host "github.com"
           :repo "awth13/org-appear")
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-modern
  :after org
  :config
  (setq org-modern-hide-stars nil
        org-modern-table-vertical 2
        org-modern-table-horizontal 1
        org-modern-checkbox nil
        org-modern-block-fringe nil
        org-modern-list nil
        org-modern-todo-faces
        '(("TODO" :foreground "#D33682" :weight bold :inverse-video t)
          ("INPROGRESS" :foreground "#859900" :weight bold :inverse-video t)
          ("BLOCKED" :foreground "#CB4B16" :weight bold :inverse-video t)
          ("ONHOLD" :foreground "#2AA198" :weight bold :inverse-video t)
          ("REVIEW" :foreground "#268BD2" :weight bold :inverse-video t)
          ("DONE" :foreground "#9FA4BB" :weight bold :inverse-video t )
          ("CANCELLED" :foreground "#574C58" :weight bold :inverse-video t)
          ("DELEGATED"  :foreground "#6c71c4" :weight bold :inverse-video t))
        org-modern-priority-faces '((?A :foreground "#DC322F" :weight bold :inverse-video t)
                                    (?B :foreground "#B58900" :weight bold :inverse-video t)
                                    (?C :foreground "#6C71C4" :weight bold :inverse-video t)))
  (global-org-modern-mode))

(use-package german-holidays)

(use-package polish-holidays
  :elpaca (polish-holidays
           :host "github.com"
           :repo "mikolajb/emacs-polish-holidays"
           :main "polish-holidays.el"))

(use-package holidays
  :elpaca nil
  ;; ;; I can't seem to get this thing to load properly. Temporary workaround.
  ;; :load-path "elpaca/repos/emacs-polish-holidays"
  :after (org-agenda polish-holidays german-holidays)
  :init
  (require 'polish-holidays)
  (require 'german-holidays)
  :config
  (setq calendar-holidays '((holiday-fixed 1 1 "New Year's Day")
                            (holiday-fixed 2 14 "Valentine's Day")
                            (holiday-fixed 4 1 "April Fools' Day")
                            (holiday-fixed 10 31 "Halloween")
                            (holiday-easter-etc)
                            (holiday-fixed 12 25 "Christmas")
                            (solar-equinoxes-solstices)
                            ustawowo-wolne-od-pracy
                            czas-letni
                            swieta-panstwowe-pozostałe-święta
                            holiday-german-holidays)))

(use-package ox-confluence-modern
  :elpaca
  (ox-confluence-modern
   :host "github.com"
   :repo "nan0scho1ar/ox-confluence-modern"
   :files ("*.el")))

(use-package dired
  :elpaca nil
  :init
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
        dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)
        dirvish-path-separators '("~" "/" "/")
        delete-by-moving-to-trash t
        ;dirvish-use-header-line nil
        dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (custom-set-faces '(dired-header ((t (:weight bold :background "unspecified" :foreground "#268bd2")))))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ("C-c o o" . dirvish-dwim)
   ("C-c o O" . dirvish)
   ("C-c o t" . dirvish-side)

   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


(use-package flymake
  :elpaca nil
  :hook (prog-mode . flymake-mode))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Major modes for text/programming
(use-package poly-ansible) ;pulls yaml-mode, ansible-mode, polymode, and allows jinja2 in yaml.
  
(use-package markdown-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package go-mode
  :mode "\\.go\\'")

(elpaca-process-queues)

(when (eq system-type 'gnu/linux)
  (add-hook 'elpaca-after-init-hook (lambda () (load "~/.config/emacs/secret/work.el" 'noerror 'nomessage))))

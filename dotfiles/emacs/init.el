;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This is my personal configuration file I use at three different environment systems - on macOS, WSL2 (wslg) and msys2.

;;; Code:

;; use-package properties order:
;; :ensure
;; :if
;; :after
;; :demand
;; :commands
;; :hooks
;; :bind[*]
;; :init
;; :config

;;;;;;;;;;;;;; HOOKATION POINT ;;;;;;;;;;;;;;
(use-package emacs
  :ensure nil
  :init
  ;; Set gc-cons high for faster startup. Values are reset at the end of elpaca init.
  (setopt gc-cons-threshold (* 1024 1024 200)
          gc-cons-percentage 0.6)

  (defun schrenker/measure-startup-time ()
    "Measure startup time."
    (message "*** Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract elpaca-after-init-time before-init-time)))
             gcs-done))

  (defun schrenker/wsl2-p ()
    "Return t if ran inside Windows Subsystem for Linux."
    (and (eq system-type 'gnu/linux)
         (string-match
          "Linux.*Microsoft.*Linux"
          (shell-command-to-string "uname -a"))))

  (when (schrenker/wsl2-p)
    (setopt emacs-appearance 'light
            browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
            browse-url-generic-args     '("/c" "start")
            browse-url-browser-function #'browse-url-generic))

  (setopt auto-revert-check-vc-info t
          auto-window-vscroll nil
          backup-by-copying t
          backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
          create-lockfiles nil
          custom-file null-device
          default-frame-alist (if (schrenker/wsl2-p)
                                  '((top . 1) (left . 1) (width . 120) (height . 40))
                                '((fullscreen . maximized) (ns-transparent-titlebar . t)))
          delete-by-moving-to-trash t
          delete-old-versions t
          delete-pair-blink-delay 0
          display-line-numbers-type 'visual
          electric-pair-open-newline-between-pairs t
          enable-recursive-minibuffers t
          frame-resize-pixelwise t
          frame-title-format '(:eval (concat user-login-name "@" system-name (if buffer-file-truename " :: %f" " :|: [%b]")))
          indent-tabs-mode nil
          inhibit-startup-message t
          inhibit-startup-screen t
          initial-major-mode 'org-mode
          initial-scratch-message nil
          kept-new-versions 6
          kept-old-versions 2
          load-prefer-newer t
          mac-command-modifier 'meta
          mac-option-modifier 'alt
          mac-right-option-modifier nil
          max-lisp-eval-depth 10000
          minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
          native-comp-async-report-warnings-errors nil
          ns-use-proxy-icon (display-graphic-p)
          read-extended-command-predicate #'command-completion-default-include-p
          require-final-newline t ;; POSIX 3.206: Definition of a 'Line'.
          savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
          scroll-conservatively 1000
          scroll-margin 10
          scroll-preserve-screen-position t
          scroll-step 1
          sentence-end-double-space nil
          set-mark-command-repeat-pop t
          tab-always-indent 'complete
          tab-width 4
          time-stamp-active t
          time-stamp-end "$"
          time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]"
          time-stamp-start "#\\+modified: [ \t]*"
          truncate-string-ellipsis "…"
          user-full-name (rot13 "Fronfgvna Mnjnqmxv")
          user-mail-address (rot13 "fronfgvna@mnjnqmxv.grpu")
          vc-follow-symlinks nil
          version-control t
          visible-bell (schrenker/wsl2-p)
          visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
          window-combination-resize t
          x-stretch-cursor t)

  (menu-bar-mode -1)
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
  (flymake-mode -1)

  (column-number-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-prettify-symbols-mode 1)
  (savehist-mode 1)
  (winner-mode 1)

  (set-language-environment 'utf-8)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (load-file (concat user-emacs-directory "lisp/general-utils.el"))

  (dolist (fn '(kmacro-call-macro
                kmacro-exec-ring-item
                apply-macro-to-region-lines))
    (advice-add fn :around #'schrenker/block-undo))

  (advice-add #'kill-buffer--possibly-save :override #'schrenker/kill-buffer--possibly-save)
  (advice-add #'completing-read-multiple :filter-args #'schrenker/crm-indicator)

  (add-hook 'emacs-startup-hook #'schrenker/measure-startup-time)
  (add-hook 'before-save-hook #'time-stamp)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode) ;; Do not allow the cursor in the minibuffer prompt

  (unbind-key (kbd "M-r"))
  (unbind-key (kbd "C-x C-n")) ; annoying
  (unbind-key (kbd "C-z")) ; suspend emacs
  (unbind-key (kbd "C-x C-z")) ; suspend frame
  (unbind-key (kbd "<f2>"))
  (unbind-key (kbd "<f10>"))

  (global-set-key (kbd "C-x k") 'schrenker/kill-this-buffer)
  (global-set-key (kbd "C-x 2") 'schrenker/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'schrenker/split-and-follow-vertically)
  (global-set-key (kbd "<A-backspace>") 'schrenker/backward-kill-word)

  (if (schrenker/wsl2-p)
      (progn
        (set-frame-font "JetBrainsMono Nerd Font 10" nil t)
        (schrenker/zoom-frame))
    (set-frame-font "JetBrainsMono Nerd Font 14" nil t)))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(if (member system-type '(windows-nt ms-dos cygwin))
    (elpaca-no-symlink-mode))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setopt use-package-always-ensure t))

(add-hook 'elpaca-ui-mode-hook (lambda ()
                                 (bind-key (kbd "/") 'elpaca-ui-search 'elpaca-ui-mode-map)))

(elpaca-wait)

(use-package exec-path-from-shell
  :demand t
  :config
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (or (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package envrc
  :if (or (executable-find "direnv") (executable-find "nix") (eq system-type 'darwin))
  :init
  (add-hook 'elpaca-after-init-hook #'envrc-global-mode -90)
  :config
  (setopt envrc-show-summary-in-minibuffer nil))

(use-package orderless
  :config
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion)))
          orderless-matching-styles '(orderless-literal
                                      orderless-regexp
                                      orderless-prefixes
                                      orderless-initialism)))

(use-package vertico
  :init
  (vertico-mode)
  (setopt vertico-scroll-margin 3
          vertico-count 15
          vertico-cycle nil))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("M-." . embark-act)
   ("C-." . embark-dwim)
   ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
                                        ;(setopt embark-verbose-indicator-display-action '(display-buffer-in-side-window (side . left)))
  (setopt embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-org
  :ensure nil
  :after embark org
  :bind (:map embark-org-item-map
              ("RET" . schrenker/org-fullcycle-checkbox)
              :map embark-org-link-map
              ("RET" . org-open-at-point)))

(use-package consult
  :demand t
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
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g F" . consult-flyspell)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-org-heading)
         ("M-g O" . consult-outline)               ;; Alternative: consult-org-heading
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

  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setopt consult-narrow-key "<"
          consult-project-function #'consult--default-project-function))

(use-package consult-project-extra
  :commands (consult-project-extra-find
             consult-project-extra-find-other-window))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("C-SPC" . corfu-insert-separator)
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setopt corfu-auto t
          global-corfu-modes '((not
                                erc-mode
                                circe-mode
                                help-mode
                                gud-mode
                                vterm-mode) t)
          corfu-cycle t
          corfu-preselect 'prompt
          corfu-count 16
          corfu-max-width 120)
  (global-corfu-mode))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :demand t
  :bind (:map corfu-popupinfo-map
              ("C-j" . corfu-popupinfo-scroll-up)
              ("C-k" . corfu-popupinfo-scroll-down))
  :config
  (setopt corfu-popupinfo-delay '(1.0 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local completion-at-point-functions
                                                '(tempel-expand
                                                  cape-elisp-symbol
                                                  t))))
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package dabbrev
  :ensure nil
  :config
  (setopt dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package transient
  :config
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-fetch "-p"
      '("-t" "Fetch all tags" ("-t" "--tags")))
    (transient-append-suffix 'magit-pull "-r"
      '("-a" "Autostash" "--autostash"))
    (transient-append-suffix `magit-diff "r"
      '("W" "Diff worktree at point" schrenker/magit-diff-with-commit-at-point))))

(use-package magit
  :bind (("C-x g" . schrenker/magit-status-rightmost-window)
         ("C-x G" . schrenker/magit-status-with-prefix)
         :map magit-status-mode-map
         ("o" . schrenker/magit-diff-visit-file-other-window)
         ("K" . magit-discard)
         ("e" . nil)
         ("E" . nil)
         ("M-j" . magit-section-forward)
         ("M-k" . magit-section-backward)
         ("M-J" . magit-section-forward-sibling)
         ("M-K" . magit-section-backward-sibling)
         ("<escape>" . meow-cancel-selection))
  :init
  (defun schrenker/magit-diff-with-commit-at-point ()
    "Invoke `magit-diff` from any magit buffer with the commit at point as its only argument. This produces a diff with the worktree."
    (interactive)
    (let* ((section (magit-current-section))
           (value (oref section value)))
      (magit-diff-range (magit-rev-parse value))))

  (defun schrenker/magit-diff-visit-file-other-window ()
    "From a diff visit the appropriate version of FILE. Display the buffer in another window."
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively 'magit-diff-visit-file)))

  (defun schrenker/magit-status-with-prefix ()
    "Prompt for a path of a repository, and view it's status.
If no repository is found, prompt user to create one."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun schrenker/magit-status-rightmost-window ()
    (interactive)
    (let ((wcount (count-windows))
          (dir (vc-root-dir)))
      (schrenker/rightmost-window-or-split)
      (when (> (count-windows) wcount)
        (set-window-parameter (get-buffer-window (current-buffer)) 'magit-dedicated t))
      (magit-status dir)))

  :config
  (setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (window-system) (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable) ;; describe-function
   ("C-h k" . helpful-key)      ;; describe-key
   ("C-h o" . helpful-symbol)   ;; describe-symbol
   ("C-h v" . helpful-variable) ;; describe-variable
   ("C-h x" . helpful-command)) ;; describe-command
  :init
  ;; Override describe commands when called interactively
  (advice-add 'describe-variable :around
              (lambda (orig-fun variable &optional buffer frame)
                (if (called-interactively-p 'interactive)
                    (helpful-variable variable)
                  (funcall orig-fun variable buffer frame))))
  (advice-add 'describe-function :around
              (lambda (orig-fun function)
                (if (called-interactively-p 'interactive)
                    (helpful-callable function)
                  (funcall orig-fun function))))
  (advice-add 'describe-symbol :around
              (lambda (orig-fun symbol &optional buffer frame)
                (if (called-interactively-p 'interactive)
                    (helpful-symbol symbol)
                  (funcall orig-fun symbol buffer frame))))
  (advice-add 'describe-command :around
              (lambda (orig-fun command)
                (if (called-interactively-p 'interactive)
                    (helpful-command command)
                  (funcall orig-fun command))))
  (advice-add 'describe-key :override 'helpful-key)

  (with-eval-after-load 'perject
    (add-hook 'helpful-mode-hook 'perject--auto-add-buffer)))

(use-package persistent-kmacro
  :ensure
  (persistent-kmacro
   :host "github.com"
   :repo "artawower/persistent-kmacro.el"))

(use-package posframe
  :if (display-graphic-p))

(use-package transpose-frame)

(use-package ace-window
  :init
  (defun schrenker/ace-swap-window ()
    "Ace-swap-window but ignore current window for the execution time."
    (interactive)
    (let ((aw-ignore-current t))
      (ace-swap-window)))

  (defun schrenker/aw-flip-window ()
    "Switch to the window you were previously in.
 If you are switching from dirvish window, make sure the root window is added to aw--window-ring."
    (interactive)
    (let ((dv (dirvish-curr)))
      (aw-flip-window)
      (if (and dv (car (dv-layout dv)))
          (aw--push-window (dv-root-window dv)))))
  :config
  (setopt aw-keys '(?e ?t ?u ?h ?o ?n ?a ?s))

  (when (display-graphic-p)
    (with-eval-after-load 'posframe
      (ace-window-posframe-mode 1))))

(use-package popper
  :init
  (defun popper-select-popup-at-bottom-maybe-hide (buffer &optional _act)
    "Display popups at the bottom of the screen.
Mark buffer as shown without showing it, if it's supposed to be suppressed."
    (if (popper--suppress-p buffer)
        (display-buffer-no-window buffer '((allow-no-window . t)))
      (popper-select-popup-at-bottom buffer _act)))
  :config
  (setopt popper-group-function #'popper-group-by-project
          popper-display-function #'popper-select-popup-at-bottom-maybe-hide
          popper-reference-buffers
          '("\\*Messages\\*"
            "\\*Warnings\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            help-mode
            helpful-mode
            compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package hydra
  :bind (("M-o" . 'hydra-uictl/body)
         ("M-O" . 'schrenker/switch-hydra)
         ("C-x C-g" . 'schrenker/smerge))
  :config
  (setopt hydra-is-helpful t
          hydra-hint-display-type 'posframe)

  (defun schrenker/switch-hydra ()
    "Switch to appropriate hydra based on mode that is currently on.
If no applicable mode is present, default to uictl."
    (interactive)
    (cond ((bound-and-true-p smerge-mode) (hydra-smerge/body))
          ((bound-and-true-p dape--process) (hydra-dape/body))
          ((bound-and-true-p git-timemachine-mode) (hydra-git-timemachine/body))
          ((eq major-mode 'org-mode) (hydra-org/body))
          (t (hydra-uictl/body))))

  (defhydra hydra-uictl
    (:hint nil)
    "

   Movement     ^^Sizing           ^^Layout            ^^Buffer
╭──────────────────────────────────────────────────────────────────^^^^^^
   [_o_] flip     [_=_]   balance    [_u_] undo          [_<_] prev
   [_O_] select   [_m_]   maximize   [_r_] redo          [_>_] next
   [_s_] swap     [_+_]   zoom in    [_2_] split down    [_b_] Buffers
   ^^             [_-_]   zoom out   [_3_] split right   [_B_] iBuffer
   ^^             [_M-k_] vShrink    [_t_] transpose     [_S_] Scratch
   ^^             [_M-j_] vEnlarge   [_x_] win delete    [_Q_] Kill
   ^^             [_M-h_] hShrink    [_X_] aw delete
   ^^             [_M-l_] hEnlarge   [_1_] single        [_q_] Quit Hydra
   ^^^^^^                                                [_TAB_] Switch
 ^^^^^^──────────────────────────────────────────────────────────────────╯
"
    ("o" schrenker/aw-flip-window)
    ("O" ace-select-window)
    ("s" schrenker/ace-swap-window)
    ("=" balance-windows)
    ("m" maximize-window)
    ("+" schrenker/zoom-frame)
    ("-" schrenker/zoom-frame-out)
    ("M-k" shrink-window)
    ("M-j" enlarge-window)
    ("M-h" shrink-window-horizontally)
    ("M-l" enlarge-window-horizontally)
    ("u" winner-undo)
    ("r" winner-redo)
    ("2" schrenker/split-and-follow-horizontally)
    ("3" schrenker/split-and-follow-vertically)
    ("t" (aw-transpose-frame (car (window-list))))
    ("x" delete-window)
    ("X" ace-delete-window)
    ("1" delete-other-windows)
    ("<" previous-buffer)
    (">" next-buffer)
    ("b" consult-buffer)
    ("B" ibuffer :color blue)
    ("S" scratch-buffer)
    ("Q" schrenker/kill-this-buffer)
    ("TAB" schrenker/switch-hydra :color blue)
    ("q" nil :color blue))

  (with-eval-after-load 'org
    (defhydra hydra-org (:hint nil)
      "

  Movement^               ^Refile^                ^Misc
╭─────────────────────────────────────────────────────────────^^^^^^
  [_K_] Prev Heading^^                             [_s_] Sort
  [_J_] Next Heading^^                             [_/_] Find
  [_b_] Tasks/Backlog      [_B_] Tasks/Backlog
  [_a_] Tasks/Active       [_A_] Tasks/Active
  [_c_] Tasks/Completed    [_C_] Tasks/Completed   [_q_] Quit Hydra
  [_n_] Notes^^                                    [_TAB_] Uictl
 ^^^^^^─────────────────────────────────────────────────────────────╯
"
      ("K" outline-previous-heading)
      ("J" outline-next-heading)
      ("b" (schrenker/org-jump-to-heading "** Backlog"))
      ("a" (schrenker/org-jump-to-heading "** Active"))
      ("c" (schrenker/org-jump-to-heading "** Completed"))
      ("n" (schrenker/org-jump-to-heading "* Notes"))
      ("B" (schrenker/refile (buffer-file-name) "Tasks/Backlog"))
      ("A" (schrenker/refile (buffer-file-name) "Tasks/Active"))
      ("C" (schrenker/refile (buffer-file-name) "Tasks/Completed"))
      ("s" (schrenker/org-sort-dwim))
      ("/" consult-org-heading)
      ("TAB" hydra-uictl/body :color blue)
      ("q" nil :color blue)))

  (with-eval-after-load 'git-timemachine
    (defhydra hydra-git-timemachine (:hint nil)
      "

 ^Rev-Movement        ^Commits^                ^Misc
╭─────────────────────────────────────────────────────────────────^^^^^^
  [_J_] Next Rev       [_b_] Blame              [_?_] Help
  [_K_] Prev Rev       [_c_] Show Commit        [_S_] Write File
  [_g_] Nth Rev        [_y_] Copy Short Hash    [_q_] Quit Hydra
  [_T_] Fuzzy Rev      [_Y_] Copy Long Hash     [_Q_] Quit Timemachine
  [_C_] Current Rev^^                           [_TAB_] Uictl
 ^^^^^^─────────────────────────────────────────────────────────────────╯
"
      ("J" git-timemachine-show-next-revision)
      ("K" git-timemachine-show-previous-revision)
      ("g" git-timemachine-show-nth-revision)
      ("T" git-timemachine-show-revision-fuzzy)
      ("C" git-timemachine-show-current-revision)
      ("b" git-timemachine-blame)
      ("c" git-timemachine-show-commit)
      ("y" git-timemachine-kill-abbreviated-revision)
      ("Y" git-timemachine-kill-revision)
      ("?" git-timemachine-help)
      ("TAB" hydra-uictl/body :color blue)
      ("S" write-file)
      ("q" nil :color blue)
      ("Q" git-timemachine-quit :color blue)))

  (with-eval-after-load 'dape
    (defhydra hydra-dape (:hint nil)
      "

  Stepping^          ^Breakpoints^             ^Info
╭─────────────────────────────────────────────────────────────^^^^^^
  [_n_] Next          [_bb_] Toggle             [_si_] Info
  [_i_] Step in       [_ba_] Add                [_sm_] Memory
  [_o_] Step out      [_bd_] Delete             [_ss_] Select Stack
  [_c_] Continue      [_bD_] Delete all         [_R_]  Repl
  [_r_] Restart       [_bl_] Set log message    [_q_]  Quit Hydra
  ^^^^                                          [_Q_]  Quit Dape
  ^^^^                                          [_TAB_] Uictl
 ^^^^^^─────────────────────────────────────────────────────────────╯
"
      ("n" dape-next)
      ("i" dape-step-in)
      ("o" dape-step-out)
      ("c" dape-continue)
      ("r" dape-restart)
      ("bb" dape-toggle-breakpoint)
      ("ba" dape-expression-breakpoint)
      ("bd" dape-remove-breakpoint-at-point)
      ("bD" dape-remove-all-breakpoints)
      ("bl" dape-log-breakpoint)
      ("si" dape-info)
      ("sm" dape-read-memory)
      ("ss" dape-select-stack)
      ("R"  dape-repl)
      ("TAB" hydra-uictl/body :color blue)
      ("q" nil :color blue)
      ("Q" dape-quit :color blue)))

  (defun schrenker/smerge ()
    "Activate smerge mode, and enter smerge hydra."
    (interactive)
    (smerge-mode 1)
    (hydra-smerge/body))

  (defhydra hydra-smerge (:hint nil)
    "

  Move^       ^Keep^^            Diff^^              Misc
╭───────────────────────────────────────────────────────────────^^^^^^^
  [_J_] Next   [_b_]   Base      [_<_] Upper/Base    [_c_] Combine
  [_K_] Prev   [_u_]   Upper     [_=_] Upper/Lower   [_r_] Resolve
 ^^            [_l_]   Lower     [_>_] Base/Lower    [_d_] Kill Current
 ^^            [_a_]   All       [_R_] Refine        [_q_] Quit Hydra
 ^^            [_RET_] Current   [_E_] Ediff         [_Q_] Quit Smerge
 ^^^^^^                                              [_TAB_] Uictl
 ^^^^^^^^───────────────────────────────────────────────────────────────╯
"
    ("J"  smerge-next)
    ("K"  smerge-prev)
    ("b"  smerge-keep-base)
    ("u"  smerge-keep-upper)
    ("l"  smerge-keep-lower)
    ("a"  smerge-keep-all)
    ("RET" smerge-keep-current)
    ("<"  smerge-diff-base-upper)
    ("="  smerge-diff-upper-lower)
    (">"  smerge-diff-base-lower)
    ("R"  smerge-refine)
    ("E"  smerge-ediff)
    ("c"  smerge-combine-with-next)
    ("r"  smerge-resolve)
    ("d"  smerge-kill-current)
    ("TAB" hydra-uictl/body :color blue)
    ("q" nil :color blue)
    ("Q" (lambda () (interactive)(smerge-auto-leave)) :color blue)))

(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("M-j" . org-metadown)
              ("M-J" . org-shiftmetadown)
              ("M-k" . org-metaup)
              ("M-K" . org-shiftmetaup)
              ("M-h" . org-metaleft)
              ("M-H" . org-shiftmetaleft)
              ("M-l" . org-metaright)
              ("M-L" . org-shiftmetaright)
              ("C-c C-j" . nil)
              ("C-c C-f" . org-format-all-headings)
              ("C-c l" . org-store-link)
              ("C-c C-^" . schrenker/org-sort-dwim))
  :init
  (defun schrenker/org-unarchive ()
    "Restore an entry that has been archived.
This function restores the entry to its original location, and
removes the ARCHIVE_TIME, ARCHIVE_FILE, ARCHIVE_OLPATH,
ARCHIVE_CATEGORY, ARCHIVE_TODO, and ARCHIVE_ITAGS properties."
    (interactive)
    (let ((orig-file (org-entry-get-with-inheritance "ARCHIVE_FILE"))
          (orig-path (org-entry-get-with-inheritance "ARCHIVE_OLPATH")))
      (org-delete-property "ARCHIVE_FILE")
      (org-delete-property "ARCHIVE_OLPATH")
      (org-delete-property "ARCHIVE_TIME")
      (org-delete-property "ARCHIVE_CATEGORY")
      (org-delete-property "ARCHIVE_TODO")
      (org-delete-property "ARCHIVE_ITAGS")
      (org-refile nil nil (list nil orig-file nil (org-find-olp `(,orig-file ,@(split-string orig-path "/")) nil)))))

  (defun schrenker/org-jump-to-heading (heading)
    "Jump to any string in the file. The purpose of this is to jump to org-mode headings.
To jump to org-mode heading, pass in literal heading, like '** Notes'."
    (interactive)
    (goto-char (point-min))
    (search-forward heading))

  (defun schrenker/org-fullcycle-checkbox ()
    "Cycle checkbox between all possible states, so [ ], [-] and [X]."
    (interactive)
    (if (org-at-item-checkbox-p)
        (if (org-list-at-regexp-after-bullet-p "\\(\\[[ ]\\]\\)[ \t]+")
            (org-toggle-checkbox '(16))
          (org-toggle-checkbox))))

  (defun schrenker/org-sort-dwim ()
    "Sort org heading. If heading is one of: [Backlog, Active, Completed], then sort by alpha -> priority -> TODO order.
Else sort by Alpha."
    (interactive)
    (cond ((string= (org-no-properties (org-get-heading t t t t)) "Notes")
           (org-sort-entries nil ?A))
          ((cl-some (lambda (x) (string= (org-no-properties (org-get-heading t t t t)) x)) '("Backlog" "Active" "Completed"))
           (org-sort-entries nil ?a)
           (org-sort-entries nil ?p)
           (org-sort-entries nil ?o))
          (t
           (org-sort-entries nil ?a))))

  (defun schrenker/get-org-template (template)
    "Fetch contents of template file from templates dir in emacs config directory."
    (with-temp-buffer
      (insert-file-contents (concat user-emacs-directory "templates/" template))
      (buffer-string)))

  (defun schrenker/refile (file headline &optional arg)
    "Refile target heading into target file, under possibly nested heading, like Tasks/Active."
    (org-refile arg nil (list nil file nil (org-find-olp `(,file ,@(split-string headline "/")) nil))))

  :config
  (load-file (concat user-emacs-directory "lisp/org-format.el"))
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  (setopt org-M-RET-may-split-line '((default . nil))
          org-archive-location "archive/%s_archive::"
          org-archive-tag "archive"
          org-default-notes-file (concat org-directory "/20221222131538-inbox.org")
          org-directory "~/org"
          org-element-archive-tag "archive"
          org-fontify-quote-and-verse-blocks t
          org-fontify-whole-heading-line t
          org-hide-emphasis-markers t
          org-insert-heading-respect-content t
          org-list-allow-alphabetical t
          org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "1)"))
          org-log-done 'time
          org-log-into-drawer "LOGBOOK"
          org-log-state-notes-insert-after-drawers t
          org-outline-path-complete-in-steps nil
          org-priority-default '?D
          org-priority-highest '?A
          org-priority-lowest  '?D
          org-priority-start-cycle-with-default t
          org-refile-use-outline-path 'file
          org-return-follows-link t
          org-tags-column -77
          org-tags-exclude-from-inheritance '("crypt" "verb" "agenda")
          org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "INPROGRESS(i!)" "BLOCKED(b@/!)" "ONHOLD(o@/!)" "REVIEW(r!)" "|" "DELEGATED(e@/@)" "CANCELLED(c@/@)" "DONE(d/@)"))
          org-use-fast-todo-selection 'expert)

  (with-eval-after-load 'org-roam
    (let ((refile-targets (schrenker/org-roam-fetch-refile-targets)))
      (setopt org-refile-targets refile-targets)))

  ;; Add note outside drawer workflow
  (defun schrenker/org-add-note (func &rest args)
    "Advisor function to go around `org-add-note'.  Takes optional
  count (c-u) and sets schrenker/org-log-into-drawer to be used by
  `schrenker/org-add-note'.

  The usage is thus:

  (advice-add 'org-log-into-drawer :around #'schrenker/org-log-into-drawer)
  (advice-add 'org-add-note :around #'schrenker/org-add-note)

  When you do not want to log note into a draw use C-u C-c C-z.
  Otherwise use C-c C-z as normal and it should log note as per
  standard `org-log-into-drawer'.
  "
    (interactive "P")
    (setq schrenker/org-log-into-drawer (car args))
    (funcall func))

  (defun schrenker/org-log-into-drawer (func)
    "Advisor function to go around `org-log-into-drawer'.
  Reads value of schrenker/org-log-into-drawer, as set by
  `schrenker/org-add-note', and if set returns nil meaning do not log
  into drawer.  Otherwise returns value from call to
  `org-log-into-draw'.  Before returning resets
  schrenker/org-log-into-drawer for subsequent calls."
    (let ((ret
           (if (not schrenker/org-log-into-drawer)
               (funcall func)
             nil)))
      (setq schrenker/org-log-into-drawer nil)
      ret))
  (setq schrenker/org-log-into-drawer nil)
  (advice-add 'org-log-into-drawer :around #'schrenker/org-log-into-drawer)
  (advice-add 'org-add-note :around #'schrenker/org-add-note)
  ;; Add note outside drawer workflow end.

  (add-hook 'org-mode-hook (lambda ()
                             (setq-local
                              completion-at-point-functions
                              (delete 'pcomplete-completions-at-point completion-at-point-functions))))
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-format-on-save-mode 1)))
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1))))

(use-package org-crypt
  :ensure nil
  :after org
  :config
  (setopt epg-pinentry-mode 'loopback
          org-crypt-disable-auto-save t
          org-crypt-key (rot13 "fronfgvna@mnjnqmxv.grpu"))
  (org-crypt-use-before-save-magic))

(use-package org-capture
  :ensure nil
  :after org
  :bind (("C-c n n" . org-capture))
  :config
  (setopt org-capture-templates `(("i" "Inbox Note" entry
                                   (file+headline org-default-notes-file "Notes")
                                   ,(schrenker/get-org-template "note")
                                   :empty-lines 1
                                   :prepend t)
                                  ("I" "Inbox Task" entry
                                   (file+olp org-default-notes-file "Tasks" "Backlog")
                                   ,(schrenker/get-org-template "task")
                                   :empty-lines 1
                                   :prepend t)
                                  ("a" "Area Note" entry
                                   (file+headline (lambda () (schrenker/org-roam-read-node-by-tag "area")) "Notes")
                                   ,(schrenker/get-org-template "note")
                                   :empty-lines 1
                                   :prepend t)
                                  ("p" "Project Note" entry
                                   (file+headline (lambda () (schrenker/org-roam-read-node-by-tag "project")) "Notes")
                                   ,(schrenker/get-org-template "note")
                                   :empty-lines 1
                                   :prepend t)
                                  ("P" "Project Task" entry
                                   (file+olp (lambda () (schrenker/org-roam-read-node-by-tag "project")) "Tasks" "Backlog")
                                   ,(schrenker/get-org-template "task")
                                   :empty-lines 1
                                   :prepend t))))

(use-package org-agenda
  :ensure nil
  :after org
  :config
  (setopt org-agenda-skip-unavailable-files t))

(use-package org-src
  :ensure nil
  :after org
  :init
  (defun schrenker/trim-src-block ()
    "Trim leading spaces in src block header and footer, and adjust contents accordingly."
    (interactive)
    (save-excursion
      (let ((element (org-element-context)))
        (when (eq (org-element-type element) 'src-block)
          (goto-char (org-element-property :begin element))
          (let* ((leading-spaces (skip-chars-forward " \t"))
                 (start (org-element-property :begin element))
                 (end (org-element-property :end element)))
            (goto-char start)
            (while (and (< (point) (- end 1))
                        (not (eobp)))
              (beginning-of-line)
              (when (looking-at (format "^ \\{0,%d\\}" leading-spaces))
                (let* ((current-leading-spaces (skip-chars-forward " \t")))
                  (beginning-of-line)
                  (delete-char
                   (if (> leading-spaces current-leading-spaces)
                       current-leading-spaces
                     leading-spaces))))
              (forward-line)))))))

  (defun schrenker/trim-src-block-buffer ()
    "Trim leading spaces in all src blocks header and footer, and adjust contents accordingly."
    (interactive)
    (org-babel-map-src-blocks nil
      (save-excursion
        (schrenker/trim-src-block))))

  :config
  (setopt org-edit-src-content-indentation 0
          org-src-preserve-indentation t
          org-babel-shell-names '("bash" "fish" "sh" "zsh" "csh" "ash" "dash" "ksh" "mksh" "posh")
          org-src-lang-modes '(("bash" . bash-ts)
                               ("shell" . bash-ts)
                               ("sh" . bash-ts)
                               ("fish" . fish)
                               ("elisp" . emacs-lisp)
                               ("sqlite" . sql)
                               ("go" . go-ts)
                               ("python" . python-ts)
                               ("py" . python-ts)
                               ("txt" . text)
                               ("typst" . typst-ts)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (python . t)))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'schrenker/trim-src-block-buffer nil t))))

(use-package org-kanban
  :after org
  :config
  (setopt org-kanban/layout '("…" . 15))
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook (lambda () (save-excursion (when (org-find-dblock "kanban") (org-update-dblock)))) nil t))))

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-modern
  :config
  (setopt org-modern-hide-stars nil
          org-modern-table nil
          org-modern-star nil
          org-modern-checkbox nil
          org-modern-block-fringe nil
          org-modern-list nil)
  (global-org-modern-mode 1))

(use-package toc-org
  :init
  (add-hook 'org-mode-hook 'toc-org-mode)
  (add-hook 'markdown-mode-hook 'toc-org-mode))

(use-package org-roam
  :commands (org-roam-capture-p)
  :after org
  :demand t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . schrenker/org-roam-node-find-nonarchived)
         ("C-c n F" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n N" . org-roam-capture)
         ("C-c n a" . org-roam-tag-add)
         ("C-c n d" . org-roam-tag-remove)
         ("C-c n c" . org-id-get-create)
         ("C-c n u" . schrenker/org-roam-update-tag-collection-nodes)
         ("C-c n s" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))

  :init
  (defun schrenker/agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (interactive)
    (setopt org-agenda-files (seq-uniq
                              (seq-map
                               #'car
                               (org-roam-db-query
                                [:select [nodes:file]
                                         :from tags
                                         :left-join nodes
                                         :on (= tags:node-id nodes:id)
                                         :where (like tag (quote "%\"agenda\"%"))])))))

  (defun schrenker/org-roam-get-all-filetags ()
    "Get all existing unique filetags from org-roam files."
    (seq-uniq (seq-map #'car
                       (org-roam-db-query [:select [tags:tag] :from tags]))))

  (defun schrenker/org-roam-get-nodes-by-tag (TAG)
    "Get all org-roam nodes, that have filetag TAG set."
    (org-roam-db-query [:select [nodes:id nodes:title]
                                :from tags
                                :left-join nodes
                                :on (= tags:node-id nodes:id)
                                :where (and (like tags:tag $s1) (not (= nodes:title $s2)))
                                :order-by [(asc title)]]
                       TAG (concat "#" TAG)))

  (defun schrenker/org-roam-read-node-by-tag (TAG)
    "Select an org-roam node from a list filtered by their filetag TAG."
    (org-roam-node-file
     (org-roam-node-read nil
                         (lambda (node) (and
                                         (member TAG (org-roam-node-tags node))
                                         (not (string= (concat "#" TAG) (org-roam-node-title node))))))))

  (defun schrenker/org-roam-update-tag-collection-nodes ()
    "Tags files are collection of links to all org-roam nodes with respective FILETAGS.
Naming format of these files are: tag:FILETAG.org. Update these files."
    (interactive)
    (let ((taglist (schrenker/org-roam-get-all-filetags)))
      (dolist (tag taglist)
        (let ((nodes (schrenker/org-roam-get-nodes-by-tag tag))
              (tagfile (concat org-directory "/tags/tag:" tag ".org")))
          (when (file-exists-p tagfile)
            (with-current-buffer (find-file-noselect tagfile)
              (goto-line 6)
              (delete-region (point) (point-max))
              (insert "\n" (mapconcat (lambda (x)
                                        (format "[[id:%s][%s]]" (car x) (cadr x)))
                                      nodes "\n"))
              (save-buffer)
              (schrenker/kill-this-buffer)))))))

  (defun schrenker/org-roam-node-find-nonarchived ()
    "Just like org-roam-node-find, find and open an Org-roam node by its title or alias. Filter out any files have 'archive' or 'tag' filetags."
    (interactive)
    (org-roam-node-find nil nil (lambda (node)
                                  (let ((tags (org-roam-node-tags node)))
                                    (if (eq tags nil)
                                        t
                                      (not (or (member "archive" tags) (member "tag" tags))))))))

  (defun schrenker/org-roam-fetch-refile-targets (&rest _)
    "Return list of org-roam nodes with filetags of 'area' or 'project', but not 'archive' or 'tag', as refile targets."
    (delq nil (mapcar (lambda (item)
                        (unless (string-match "/\\(archive\\|tags\\)/" item) `(,item :maxlevel . 3)))
                      (seq-uniq
                       (seq-map
                        #'car
                        (org-roam-db-query
                         [:select [nodes:file]
                                  :from tags
                                  :left-join nodes
                                  :on (= tags:node-id nodes:id)
                                  :where (or (like tag (quote "%\"area\"%"))
                                             (like tag (quote "%\"project\"%")))]))))))

  :config
  (setopt schrenker/org-fileslug "%<%Y%m%d%H%M%S>-${slug}.org"
          org-roam-capture-templates `(("p" "Project")
                                       ("pp" "Minor Project" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "project-minor"))
                                        :immediate-finish t :unnarrowed t)
                                       ("pP" "Major Project" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "project-major"))
                                        :immediate-finish t :unnarrowed t)
                                       ("a" "Area" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "area"))
                                        :immediate-finish t :unnarrowed t)
                                       ("r" "Resource")
                                       ("rr" "Resource" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "resource"))
                                        :immediate-finish t :unnarrowed t)
                                       ("rc" "Culinary" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "resource-culinary"))
                                        :immediate-finish t :unnarrowed t)
                                       ("ri" "Investigation" plain "%?"
                                        :target (file+head
                                                 ,schrenker/org-fileslug
                                                 ,(schrenker/get-org-template "resource-investigation"))
                                        :immediate-finish t :unnarrowed t))
          org-roam-directory org-directory
          org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))

  (with-eval-after-load 'org-agenda
    (schrenker/agenda-files-update)
    (advice-add 'org-agenda :before #'schrenker/agenda-files-update)
    (advice-add 'org-todo-list :before #'schrenker/agenda-files-update))

  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after org-roam
  :demand t
  :bind (("C-c n b" . consult-org-roam-backlinks)
         ("C-c n w" . consult-org-roam-forward-links))
  :config
  (setopt consult-org-roam-grep-func #'consult-ripgrep
          consult-org-roam-buffer-enabled nil)
  (consult-org-roam-mode 1)
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))

(use-package ox-confluence-modern
  :ensure
  (ox-confluence-modern
   :host "github.com"
   :repo "nan0scho1ar/ox-confluence-modern"
   :files ("*.el")))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("J" . ibuffer-jump-to-buffer)
         ("M-o" . nil))
  :config
  (setopt ibuffer-eliding-string "…"
          ibuffer-jump-offer-only-visible-buffers t)
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("M-+" . dired-create-empty-file)
        ("J" . dired-goto-file))
  :init
  ;; aw-flip-window dired workflow start
  (defvar schrenker/last-dired-window-before-jump nil)

  (defun schrenker/dired-find-file-other-window (orig-fun &rest args)
    "Save current window, call ORIG-FUN with ARGS, and add saved window to aw-window-ring.

Purpose of this is to be able to go back to Dired window with aw-flip-window, if Dired window was left by visiting the file in other window."
    (setq schrenker/last-dired-window-before-jump (get-buffer-window))
    (let ((result (apply orig-fun args)))
      (when schrenker/last-dired-window-before-jump
        (ring-insert aw--window-ring schrenker/last-dired-window-before-jump))
      result))

  (advice-add 'dired-find-file-other-window :around #'schrenker/dired-find-file-other-window)
  ;; aw-flip-window dired workflow end

  (add-hook 'dired-mode-hook
            (lambda ()
              (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                                        ace-display-buffer))
                          aw-ignore-current t)
              (display-line-numbers-mode -1)))

  :config
  (setopt dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
          dired-dwim-target t)

  (when (eq system-type 'darwin)
    (setopt insert-directory-program "/opt/homebrew/bin/gls"
            dired-listing-switches "-aBhl --group-directories-first")))

(use-package dired-x
  :ensure nil
  :config
  (setopt dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store"))
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package dirvish
  :bind
  (("C-c f" . dirvish-fd)
   ("C-c F" . schrenker/dirvish-fd-project)
   ("C-x d" . schrenker/mini-dirvish-here)
   ("C-x C-d" . dirvish)
   :map dirvish-mode-map
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-jump)
   ("s"   . dirvish-quicksort)
   ("v"   . dirvish-vc-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-l" . dirvish-history-go-forward)
   ("M-h" . dirvish-history-go-backward)
   ("M-L" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-T" . dirvish-layout-switch))
  :init
  (defun schrenker/dirvish-fd-project ()
    "Run dirvish-fd in the root of current vc project. If not in vc project, run in current directory."
    (interactive)
    (dirvish-fd (dirvish--get-project-root) nil))

  (defun schrenker/mini-dirvish-here (&optional path)
    "Open dired version of Dirvish in current window, without prompting for directory."
    (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
    (dirvish--reuse-or-create
     path (when dirvish--this (car (dv-layout dirvish--this)))))

  (dirvish-override-dired-mode)

  :config
  (dirvish-side-follow-mode)
  (setopt dirvish-attributes '(vc-state subtree-state nerd-icons collapse file-size)
          dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
          dirvish-path-separators '("~" "/" "/")
          dirvish-subtree-state-style 'nerd
          dirvish-subtree-prefix " |"
          dirvish-default-layout '(1 0.1 0.6)
          dirvish-layout-recipes '((0 0.0 0.5) (0 0.0 0.8) (1 0.08 0.8) (1 0.1 0.6))
          dirvish-cache-dir (expand-file-name ".cache/dirvish/" user-emacs-directory)
          dirvish-preview-environment '((inhibit-message . t)
                                        (non-essential . t)
                                        (delay-mode-hooks . nil)
                                        (enable-dir-local-variables . nil)
                                        (enable-local-variables . :safe)))

  (with-eval-after-load 'dirvish-fd
    (setq dirvish-fd-bufname "fd Q:[%s] D:[%s] %s")))

(use-package eat
  :ensure (eat :type git
               :host "codeberg.org"
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :bind
  (("C-c v" . eat-project)
   ("C-c V" . eat-project-other-window)
   :map eat-mode-map
   ("C-c C-c" . eat-self-input)
   ("C-d" . eat-self-input)
   ("RET" . schrenker/eat-ret-dwim))

  :init
  (defun schrenker/line-of-current-prompt ()
    "Get the prompt line of current eat buffer, and save it to a variable."
    (save-excursion
      (goto-char (point-max))
      (setq-local schrenker/eat-prompt-line (array-current-line))))

  (defun schrenker/prompt-line-p ()
    "Check if point is at prompt line or not. Do it by comparing to variable set by schrenker/line-of-current-prompt function."
    (eq (array-current-line) schrenker/eat-prompt-line))

  (defun schrenker/eat-ret-dwim ()
    "If currently in insert mode, send RET. If not in insert mode, enter insert mode, and go to the end of the command, regardless of cursor position."
    (interactive)
    (if eat--semi-char-mode
        (eat-self-input 1)
      (progn
        (goto-line (point-max))
        (eat-self-input 1 ?\C-a)
        (eat-self-input 1 ?\C-e)
        (call-interactively #'meow-insert))))

  :config
  (setopt eat-kill-buffer-on-exit t)
  (with-eval-after-load 'perject
    (add-hook 'eat-mode-hook 'perject--auto-add-buffer))
  (with-eval-after-load 'meow
    (push '(eat-mode . insert) meow-mode-state-list)
    (add-hook 'eat-mode-hook
              (lambda ()
                (add-hook 'meow-insert-enter-hook
                          (lambda ()
                            (when eat-terminal ;; throws "Process is not running" without this
                              (eat-semi-char-mode)))
                          nil t)
                (add-hook 'meow-insert-exit-hook
                          (lambda ()
                            (eat-emacs-mode)
                            (schrenker/line-of-current-prompt))
                          nil t))))
  (add-hook 'eat-mode-hook (lambda ()
                             (display-line-numbers-mode -1)
                             (corfu-mode -1)
                             (vi-tilde-fringe-mode -1))))

(use-package pdf-tools
  :init
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package tempel
  :bind (:map tempel-map
              ("M-j" . tempel-next)
              ("M-k" . tempel-previous))

  :init
  (setopt tempel-path (concat user-emacs-directory "templates/tempel"))
  (defun tempel-setup-capf ()
    "Add the Tempel Capf to `completion-at-point-functions' before the main programming mode Capf, such that it will be tried first."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package buffer-name-relative
  :init
  (defun schrenker/buffer-name-relative--abbrev-directory-impl (path overflow)
    "Abbreviate PATH by OVERFLOW characters. Omitted characters are replaced by the value of truncate-string-ellipsis variable."
    (let ((beg (string-match-p "[^/]" path)))
      (cond
       (beg
        (let ((end (string-search "/" path beg)))
          (cond
           (end
            (setq beg (1+ beg))
            (let ((len 1)
                  (trunc (- end beg)))
              (setq overflow (- overflow trunc))
              (when (< overflow 0)
                (setq beg (- beg overflow))
                (setq trunc (+ trunc overflow))
                (setq len (- len overflow))
                (setq overflow 0))
              (cons
               ;; The `head'.
               (cond
                ((< 1 len)
                 (concat (substring path 0 (1- beg)) truncate-string-ellipsis))
                (t
                 (substring path 0 beg)))
               ;; The `tail'.
               (cond
                ((zerop overflow)
                 (cons (substring path end) nil))
                (t
                 (buffer-name-relative--abbrev-directory-impl (substring path end) overflow))))))
           (t ;; `end' not found.
            (cons path nil)))))
       (t ;; `beg' not found.
        (cons path nil)))))

  (defun schrenker/prepend-path-with-vc-dir (&rest r)
    (let ((vc-root (buffer-name-relative-root-path-from-vc (nth 1 r))))
      (if vc-root
          (setq buffer-name-relative-prefix
                (concat "["
                        (let ((vc-dir (file-name-nondirectory (directory-file-name (buffer-name-relative-root-path-from-vc (nth 1 r))))))
                          (if (length> vc-dir 12)
                              (concat (substring vc-dir 0 6) ".." (substring vc-dir -4))
                            vc-dir))
                        "]:"))
        (setq buffer-name-relative-prefix ""))))

  :config
  (setopt buffer-name-relative-abbrev-limit 24)
  (advice-add 'buffer-name-relative--abbrev-directory-impl
              :override
              #'schrenker/buffer-name-relative--abbrev-directory-impl)
  (advice-add 'buffer-name-relative--create-file-buffer-advice
              :before
              #'schrenker/prepend-path-with-vc-dir)
  (buffer-name-relative-mode))

(use-package vundo
  :bind
  ("M-v" . vundo))

(use-package goggles
  :config
  (setopt goggles-pulse t)
  (add-hook 'prog-mode-hook #'goggles-mode)
  (add-hook 'text-mode-hook #'goggles-mode))

(use-package prism
  :config
  ;; Needed before https://github.com/alphapapa/prism.el/issues/22 is fixed.
  (unless (display-graphic-p)
    (load (concat user-emacs-directory "lisp/prism-cl.el") 'noerror 'nomessage))
  (setopt prism-comments nil
          prism-whitespace-mode-indents '((yaml-mode . yaml-indent-offset)
                                          (python-ts-mode . python-indent-offset)
                                          (t . 2)))
  (add-hook 'yaml-mode-hook #'prism-whitespace-mode)
  (add-hook 'bash-ts-mode-hook #'prism-whitespace-mode)
  (add-hook 'shell-script-mode-hook #'prism-whitespace-mode)
  (add-hook 'python-ts-mode-hook #'prism-whitespace-mode)
  (add-hook 'emacs-lisp-mode-hook #'prism-mode))

(use-package mood-line
  :init
  (mood-line-mode))

(use-package solaire-mode
  :init
  (solaire-global-mode))

(use-package solarized-theme
  :demand t
  :init
  (setopt solarized-distinct-doc-face t
          solarized-distinct-fringe-background t
          solarized-height-minus-1 1.0
          solarized-height-plus-1 1.0
          solarized-height-plus-2 1.0
          solarized-height-plus-3 1.0
          solarized-height-plus-4 1.0
          solarized-scale-org-headlines nil
          solarized-scale-outline-headlines nil
          solarized-use-variable-pitch nil
          solarized-use-more-italic t)
  (load-file (concat user-emacs-directory "lisp/solarized-overlay.el"))

  :config
  (schrenker/setup-theme))

(use-package nerd-icons
  :config
  (setopt nerd-icons-font-family "JetBrainsMono Nerd Font"
          nerd-icons-scale-factor 1.2)
  (add-to-list 'nerd-icons-extension-icon-alist '("jar"           nerd-icons-devicon "nf-dev-java"       :face nerd-icons-dpurple))
  (add-to-list 'nerd-icons-extension-icon-alist '("jenkinsfile"   nerd-icons-devicon "nf-dev-jenkins"    :face nerd-icons-dpurple))
  (add-to-list 'nerd-icons-extension-icon-alist '("groovy"        nerd-icons-devicon "nf-dev-groovy"     :face nerd-icons-cyan))
  (add-to-list 'nerd-icons-extension-icon-alist '("org_archive"   nerd-icons-sucicon "nf-custom-orgmode" :face nerd-icons-dgreen))

  (add-to-list 'nerd-icons-regexp-icon-alist '("postgresql.conf$" nerd-icons-devicon "nf-dev-postgresql" :face nerd-icons-blue))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^flake.lock$"     nerd-icons-mdicon  "nf-md-nix"         :face nerd-icons-dblue))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^nix.conf$"       nerd-icons-mdicon  "nf-md-nix"         :face nerd-icons-dpink))
  (add-to-list 'nerd-icons-regexp-icon-alist '("Jenkinsfile\\'"   nerd-icons-sucicon "nf-seti-jenkins"   :face nerd-icons-cyan-alt))

  (setcdr (assoc "nf-dev-go" nerd-icons/devicon-alist) "󰟓"))

(use-package nerd-icons-ibuffer
  :config
  (setopt nerd-icons-ibuffer-icon-size 1.1
          nerd-icons-ibuffer-formats `((mark
                                        modified
                                        read-only
                                        ,(if (>= emacs-major-version 26) 'locked "")
                                        " "
                                        (icon 2 2)
                                        " "
                                        (name 48 48 :left :elide)
                                        " "
                                        (size-h 9 -1 :right)
                                        " "
                                        (mode+ 16 16 :left :elide)
                                        " "
                                        filename-and-process+)
                                       (mark
                                        modified
                                        read-only
                                        ,(if (>= emacs-major-version 26) 'locked "")
                                        " "
                                        (icon 2 2)
                                        " "
                                        (name 30 30 :left :elide)
                                        " "
                                        filename-and-process+)))
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package kind-icon
  :after corfu
  :config
  (setopt kind-icon-blend-background nil
          kind-icon-default-face 'corfu-default
          kind-icon-use-icons nil
          kind-icon-mapping
          `((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
            (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
            (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
            (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
            (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
            (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
            (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
            (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
            (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
            (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
            (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
            (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
            (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
            (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
            (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
            (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
            (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
            (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
            (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
            (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
            (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
            (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
            (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
            (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
            (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
            (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
            (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
            (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
            (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
            (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
            (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
            (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
            (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package ligature
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures '(prog-mode yaml-mode) '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                                   "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                                   "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                                   "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                                   "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                                   "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                                   ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                                   "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                                   "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                                   "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                                   "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package verb
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(use-package wgrep
  :config
  (setopt wgrep-too-many-file-length 50))

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

(use-package expand-region
  :bind ("C-t" . er/expand-region)
  :init
  (setopt er/try-expand-list
          '(er/mark-inside-quotes
            er/mark-outside-quotes
            er/mark-inside-pairs
            er/mark-outside-pairs)))

(use-package surround
  :bind-keymap ("M-t" . surround-keymap))

;;;;;;;;;;;;;; CURATION POINT ;;;;;;;;;;;;;;
(use-package woman
  :ensure nil
  :bind
  (:map woman-mode-map
        ("M-j" . Man-next-section)
        ("M-k" . Man-previous-section)
        ("/" . meow-visit)
        ("n" . schrenker/meow-search)
        ("N" . schrenker/meow-search-backwards)
        (";" . meow-reverse)
        ("J" . Man-goto-section)))

(use-package info
  :ensure nil
  :bind
  (:map Info-mode-map
        ("M-j" . Info-next)
        ("M-k" . Info-prev)))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure eglot-inlay-hints-mode)
  :bind
  (("C-c c c" . eglot)
                                        ;("C-c c f" . eglot-format)
   ("C-c c a" . eglot-code-actions)
   ("C-c c r" . eglot-rename)
   ("C-c c i" . eglot-find-implementation)
   ("C-c c d" . eglot-find-declaration)
   ("C-c c t" . eglot-find-typeDefinition))
  :init
  (setopt eglot-autoshutdown t)
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  :config
  (add-to-list 'eglot-workspace-configuration
               '(:yaml . (schemas .
                                  ((https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/schemas/v3.0/schema.json "/*")))))

  (defun schrenker/eglot-capf ()
    (setq-local completion-at-point-functions
                (list #'eglot-completion-at-point
                      #'cape-file
                      #'tempel-expand)))
  (add-hook 'eglot-managed-mode-hook #'schrenker/eglot-capf))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("M-g c" . consult-eglot-symbols)))

(use-package dape
  :disabled
  :ensure
  (dape
   :host "github.com"
   :repo "svaante/dape"
   :pin t)
  :config
  ;; Add inline variable hints, this feature is highly experimental
  ;; (setq dape-inline-variables t)

  ;; To remove info buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)

  ;; To remove repl buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Use n for next etc. in REPL
  ;; (setq dape-repl-use-shorthand t)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 command-cwd dape-cwd-fn
                 host "127.0.0.1"
                 port 55878
                 :type "debug"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-cwd-fn))

  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode python-mode)
                 command "python3"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default)))

(use-package treesit
  :ensure nil
  :init
  (setopt treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
            (c "https://github.com/tree-sitter/tree-sitter-c")
            (cmake "https://github.com/uyha/tree-sitter-cmake")
            (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp")
            (go "https://github.com/tree-sitter/tree-sitter-go")
            (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (lua "https://github.com/Azganoth/tree-sitter-lua")
            (make "https://github.com/alemuller/tree-sitter-make")
            (markdown "https://github.com/ikatyang/tree-sitter-markdown")
            (python "https://github.com/tree-sitter/tree-sitter-python")
            (r "https://github.com/r-lib/tree-sitter-r")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (typst . ("https://github.com/uben0/tree-sitter-typst" "master" "src"))
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master" "src")))))

(use-package typst-ts-mode
  :disabled
  :ensure (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :init
  (setopt typst-ts-mode-watch-options "--open"))

(use-package typst-preview
  :disabled
  :after typst-ts-mode
  :ensure
  (typst-preview
   :host "github.com"
   :repo "havarddj/typst-preview.el"))

;; Major modes for text/programming
(use-package poly-ansible) ;pulls yaml-mode, ansible-mode, polymode, and allows jinja2 in yaml.

(use-package yaml-mode)

(use-package yaml-pro
  :demand t
  :bind
  (:map yaml-pro-mode-map
        ("C-c C-'" . yaml-pro-edit-scalar)
        ("C-c '" . nil)
        ("M-J" . yaml-pro-move-subtree-down)
        ("M-K" . yaml-pro-move-subtree-up)
        ("M-j" . yaml-pro-next-subtree)
        ("M-k" . yaml-pro-prev-subtree)
        ("M-h" . yaml-pro-unindent-subtree)
        ("M-l" . yaml-pro-indent-subtree)
        ("M-?" . yaml-pro-convolute-tree))
  :init
  (add-hook 'yaml-mode-hook 'yaml-pro-mode))

(use-package markdown-mode)

(use-package nix-mode)

(use-package nginx-mode)

(use-package format-all
  :init
  (add-hook 'prog-mode-hook 'format-all-mode)
  :config
  (add-hook 'go-ts-mode-hook (lambda ()
                               (add-to-list 'format-all-formatters '("Go" gofmt goimports)))))

(use-package powershell)

(use-package ob-powershell
  :after (powershell org))

(use-package bicep-mode
  :ensure
  (bicep-mode
   :host "github.com"
   :repo "christiaan-janssen/bicep-mode"))

(use-package prometheus-mode)

(use-package promql-mode
  :ensure
  (promql-mode
   :host "github.com"
   :repo "Andor/promql-mode"))

(use-package go-mode
  :after eglot
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (setopt go-ts-mode-indent-offset 4)
  (add-hook 'go-ts-mode-hook (lambda ()
                                        ;           (eglot-inlay-hints-mode 1)
                               (go-eldoc-setup)
                               (setq-local tab-width 4)
                               (setq-local indent-tabs-mode 1)))
  :config
  (setcdr (assoc '(go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) eglot-server-programs)
          '("gopls" :initializationOptions
            (:hints (
                     :parameterNames t
                     :rangeVariableTypes t
                     :functionTypeParameters t
                     :assignVariableTypes t
                     :compositeLiteralFields t
                     :compositeLiteralTypes t
                     :constantValues t))))
  (setopt eglot-workspace-configuration
          '((:gopls .
                    ((staticcheck . t)
                     (matcher . "CaseSensitive"))))))

(use-package go-eldoc)

(use-package go-guru)

(use-package gorepl-mode)

(use-package go-tag)

(use-package go-gen-test)

(use-package apparmor-mode)

(use-package python-mode
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (setopt python-indent-offset 4)
  (add-hook 'python-ts-mode-hook (lambda ()
                                        ;(eglot-inlay-hints-mode 1)
                                   (setq-local tab-width 4))))

(use-package json-mode)

(use-package rego-mode)

(use-package fish-mode)

(use-package fish-completion
  :config
  (global-fish-completion-mode))

(use-package bash-completion)

(use-package sh-script
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (setopt sh-shell "bash")
  (setopt sh-shell-file "/bin/bash")
  :mode
  ("\\.sh\\'" . bash-ts-mode))

(use-package dockerfile-mode)

(use-package jenkinsfile-mode
  :mode "\\.jenkinsfile\\'")

(use-package d2-mode)

(use-package ob-d2)

(use-package flycheck
  :config
  (add-hook 'elpaca-after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (setopt flycheck-eglot-exclusive t)
  (global-flycheck-eglot-mode 1))

(use-package consult-flycheck)

(use-package flyspell
  :ensure nil
  :config
  (setopt ispell-program-name "aspell"
          ispell-silently-savep t)
  (add-hook 'org-mode-hook #'flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :config
  (unbind-key (kbd "C-c $") 'flyspell-mode-map))

(use-package consult-flyspell)

(use-package meow
  :config
  (load-file (concat user-emacs-directory "lisp/meovil.el"))
  (global-unset-key (kbd "C-c SPC"))
  (add-to-list 'meow-mode-state-list '(elpaca-ui-mode . motion))
  (add-to-list 'meow-mode-state-list '(dired-mode . motion))
  (add-to-list 'meow-mode-state-list '(dirvish-mode . motion))
  (add-to-list 'meow-mode-state-list '(ibuffer-mode . motion))
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  (defun schrenker/meow-old-quit ()
    "Quit current window or buffer."
    (interactive)
    (if (> (seq-length (window-list (selected-frame))) (if (dirvish-side--session-visible-p) 2 1))
        (delete-window)
      (previous-buffer)))

  (when (schrenker/wsl2-p)
    (defun schrenker/wsl-copy-region-to-clipboard (start end)
      "Copy region to Windows clipboard."
      (interactive "r")
      (call-process-region start end "clip.exe" nil 0)
      (call-interactively #'meow-cancel-selection))

    (defun schrenker/wsl-kill-region-to-clipboard (start end)
      "Copy region to Windows clipboard."
      (interactive "r")
      (call-process-region start end "clip.exe" nil 0)
      (call-interactively #'kill-region))

    (defun schrenker/wsl-clipboard-to-string ()
      "Return Windows clipboard as string."
      (let ((coding-system-for-read 'dos))
        (substring              ; remove added trailing \n
         (replace-regexp-in-string "\r" ""
                                   (shell-command-to-string "powershell.exe -Command Get-Clipboard")) 0 -1)))

    (defun schrenker/wsl-paste-from-clipboard (arg)
      "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
      (interactive "P")
      (let ((clip (schrenker/wsl-clipboard-to-string)))
        (if current-prefix-arg
            (save-excursion
              (insert clip))
          (insert clip))
        (if arg (kill-new clip))))

    (global-set-key (kbd "M-w") 'schrenker/wsl-copy-region-to-clipboard)
    (global-set-key (kbd "C-w") 'schrenker/wsl-kill-region-to-clipboard)
    (global-set-key (kbd "C-v") 'schrenker/meow-yank-forward)
    (global-set-key (kbd "C-y") 'schrenker/wsl-paste-from-clipboard))

  (global-set-key (kbd "M-p") 'meow-yank-pop)
  (global-set-key (kbd "C-o") (lambda ()
                                (interactive)
                                (let ((current-prefix-arg '(4))) ;; emulate C-u
                                  (call-interactively 'set-mark-command))))


  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))

  (setopt meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-motion-overwrite-define-key
   '("j" . schrenker/meow-next)
   '("k" . schrenker/meow-prev)
   '("<escape>" . nil)
   '("SPC" . nil)
   '("SPC SPC" . consult-project-extra-find)
   '("S-SPC S-SPC" . consult-project-extra-find-other-window)
   '("SPC ." . popper-toggle)
   '("SPC ," . popper-cycle)
   '("SPC '" . popper-toggle-type)
   '("SPC f" . flyspell-correct-wrapper))

  ;;Disable
  (meow-normal-define-key
   '("e" . ignore)
   '("E" . ignore)
   '("q" . ignore)
   '("SPC" . nil))

  ;;Leader
  (meow-normal-define-key
   '("SPC SPC" . consult-project-extra-find)
   '("S-SPC SPC" . consult-project-extra-find-other-window)
   '("SPC ." . popper-toggle)
   '("SPC ," . popper-cycle)
   '("SPC '" . popper-toggle-type)
   '("SPC f" . flyspell-correct-wrapper))

  ;;Movement
  (meow-normal-define-key
   '("/" . meow-visit)
   '(":" . meow-goto-line)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("f" . meow-find)
   '("F" . schrenker/meow-find-backwards)
   '("h" . schrenker/meow-left)
   '("H" . windmove-left)
   '("j" . schrenker/meow-next)
   '("k" . schrenker/meow-prev)
   '("l" . schrenker/meow-right)
   '("L" . windmove-right)
   '("t" . meow-till)
   '("T" . schrenker/meow-till-backwards)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol))

  ;;Insert
  (meow-normal-define-key
   '("a" . schrenker/meow-smart-append)
   '("A" . schrenker/meow-append-to-eol)
   '("i" . meow-insert)
   '("I" . schrenker/meow-insert-at-bol)
   '("o" . meow-open-below)
   '("O" . meow-open-above))

  ;;Modify
  (meow-normal-define-key
   '("'" . meow-query-replace)
   '("\"" . meow-query-replace-regexp)
   '("c" . schrenker/meow-change)
   '("C" . schrenker/meow-change-to-eol)
   '("d" . schrenker/meow-kill)
   '("D" . schrenker/meow-kill-to-eol)
   '("J" . schrenker/meow-join-below)
   '("s" . meow-change)
   '("S" . schrenker/meow-change-to-eol)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("x" . meow-delete)
   '("X" . meow-backward-delete))

  ;;Clipboard
  (meow-normal-define-key
   '("p" . schrenker/meow-yank-forward)
   '("P" . schrenker/meow-yank)
   '("y" . schrenker/meow-copy))

  ;;Selection
  (meow-normal-define-key
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   ;'("%" . er/expand-region)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("v" . schrenker/meow-visual)
   '("V" . meow-line)
   '("Y" . schrenker/meow-copy-to-eol)
   '("z" . meow-pop-selection)
   '("<escape>" . meow-cancel-selection))

  ;;Misc
  (meow-normal-define-key
   '("0" . schrenker/meow-expand-or-digit-argument)
   '("1" . schrenker/meow-expand-or-digit-argument)
   '("2" . schrenker/meow-expand-or-digit-argument)
   '("3" . schrenker/meow-expand-or-digit-argument)
   '("4" . schrenker/meow-expand-or-digit-argument)
   '("5" . schrenker/meow-expand-or-digit-argument)
   '("6" . schrenker/meow-expand-or-digit-argument)
   '("7" . schrenker/meow-expand-or-digit-argument)
   '("8" . schrenker/meow-expand-or-digit-argument)
   '("9" . schrenker/meow-expand-or-digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("K" . helpful-at-point)
   '("n" . schrenker/meow-search)
   '("N" . schrenker/meow-search-backwards)
   '("Q" . schrenker/meow-old-quit))


  (with-eval-after-load 'persistent-kmacro
    (meow-normal-define-key
     '("SPC mm" . persistent-kmacro-execute-macro)
     '("SPC ma" . persistent-kmacro-name-last-kbd-macro)
     '("SPC mr" . persistent-kmacro-remove-macro)
     '("SPC ms" . persistent-kmacro-save-session)
     '("SPC ml" . persistent-kmacro-restore-sesstion)))

  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (add-hook 'meow-switch-state-hook (lambda (&rest _) (when (symbol-value 'meow-beacon-mode) (corfu-quit))))

  (setopt meow-use-clipboard t
          meow-use-cursor-position-hack t
          meow-expand-exclude-mode-list nil
          meow-use-enhanced-selection-effect t
          meow-select-on-change nil
          meow-motion-remap-prefix "A-C-M-"
          meow-char-thing-table '((?r . round)
                                  (?s . square)
                                  (?c . curly)
                                  (?a . angle)
                                  (?S . string)
                                  (?o . symbol)
                                  (?w . window)
                                  (?b . buffer)
                                  (?p . paragraph)
                                  (?l . line)
                                  (?d . defun)
                                  (?. . sentence)))

  (meow-global-mode 1))

(use-package perject
  :demand t
  :config
  (setopt perject-load-at-startup 'all
          perject-save-frames '(nil)
          perject-frame-title-format nil
          perject-switch-to-new-collection t
          perject-save-on-exit 'all
          perject-reload-default '(keep t)
          perject-close-default '(t nil t)
          perject-delete-default '(nil t nil t))


  (with-eval-after-load 'dirvish
    (advice-add 'perject-switch :before
                (lambda (&rest r) (let ((visible (dirvish-side--session-visible-p)))
                                    (when (eq visible (selected-window))
                                      (other-window 1))))))


  (defun schrenker/perject-switch-project-global ()
    "Shows unfiltered list of all collections and projects to switch between them freely"
    (interactive)
    (let ((current-prefix-arg '(4))) ;; emulate C-u
      (call-interactively 'perject-switch)))

  (defun schrenker/perject-switch-collection ()
    (interactive)
    (schrenker/call-negative 'perject-switch))

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'perject--previous-collections))

  (perject-mode 1)

  (defvar schrenker/perject-loaded-buffer-list nil)
  (defvar schrenker/perject-visited-buffer-list nil)

  (defun schrenker/add-to-sublist (key element list)
    "Add ELEMENT to the sublist in LIST identified by KEY, if it doesn't already exist."
    (let* ((sublist (assoc key list))  ; Find the sublist
           (buffers (cadr sublist)))   ; Get the second element of sublist
      (unless (member element buffers)  ; Check if the element exists
        (if (and sublist buffers)  ; Check if sublist and buffers are non-nil
            (setcar (cdr sublist) (cons element buffers))  ; If yes, append element
          (setcdr sublist (list (list element)))))))

  (defun schrenker/perject-get-loaded-buffer-list ()
    (let ((collections (perject-get-collections)))
      (dolist (col collections)
        (let ((projects (perject-get-projects col)))
          (dolist (pr projects)
            (add-to-list 'schrenker/perject-loaded-buffer-list `(,pr . ,(list (perject-get-buffers pr))))
            (add-to-list 'schrenker/perject-visited-buffer-list `(,pr . ())))))))

  (defun schrenker/perject-kill-unused-buffers ()
    (let ((loaded schrenker/perject-loaded-buffer-list)
          (vis schrenker/perject-visited-buffer-list))
      (dolist (l loaded)
        (let ((header (car l))
              (body (cadr l)))
          (unless (eq (cadr (assoc header vis)) nil)
            (dolist (b body)
              (unless (member b (cadr (assoc header vis)))
                (ignore-errors (perject-remove-buffer-from-project b header))
                (when (perject-anonymous-buffer-p b)
                  (message "Perject cleanup: %s killed in %s" b header)
                  (kill-buffer b))))))))
    (perject-save (perject-get-collections)))

  (add-hook 'kill-emacs-hook #'schrenker/perject-kill-unused-buffers)

  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (when (and (not (bound-and-true-p perject-collections)) (not (eq perject-load-at-startup nil)))
                (perject--init)
				(schrenker/perject-get-loaded-buffer-list)
                (add-hook 'buffer-list-update-hook
                          (lambda ()
                            (when (and (perject-current) (equal (car (buffer-local-value 'perject-buffer (car (buffer-list)))) (perject-current)))
                              (when (assoc (perject-current) schrenker/perject-visited-buffer-list)
                                (schrenker/add-to-sublist (perject-current) (car (buffer-list)) schrenker/perject-visited-buffer-list))))))))

  :bind
  (:map perject-mode-map
        ("C-<tab> c" . perject-create)
        ("C-<tab> r" . perject-rename)
        ("C-<tab> R" . perject-rename-collection)
        ("C-<tab> K" . perject-delete)
        ("C-<tab> e" . perject-open-close-or-reload)
        ("C-<tab> s" . perject-sort)
        ("C-<tab> n" . perject-next-project)
        ("C-<tab> p" . perject-previous-project)
        ("C-<tab> N" . perject-next-collection)
        ("C-<tab> P" . perject-previous-collection)
        ("C-<tab> C-<tab>" . schrenker/perject-switch-project-global)
        ("C-<tab> TAB" . perject-switch)
        ("C-<tab> a" . perject-add-buffer-to-project)
        ("C-<tab> d" . perject-remove-buffer-from-project)
        ("C-<tab> w" . perject-save)))

(use-package perject-consult
  :ensure
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
  :ensure
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
  :ensure
  (perject-tab
   :host "github.com"
   :repo "overideal/perject"
   :main "perject-tab.el")
  :after perject
  :init
  (setq-default perject-tab-states '(("mutable" always "⟨" "⟩")
                                     ("dynamic" perject-tab--dynamic-state "[" "]")
                                     ("immutable" ignore "⟦" "⟧")))
  (perject-tab-mode 1)
  (add-hook 'perject-before-switch-hook (lambda (&rest orig new frame)
                                          (let ((inhibit-message t)
                                                (message-log-max nil))
                                            (when (cdr (perject-current))
                                              (unless (perject-tab-tabs)
                                                (perject-tab-create))
                                              (call-interactively #'perject-tab-set)))))
  :bind
  (:map perject-tab-mode-map
        ("C-<tab> t o" . perject-tab-recent)
        ("C-<tab> t p" . perject-tab-previous)
        ("C-<tab> t n" . perject-tab-next)
        ("C-<tab> t S" . perject-tab-set)
        ("C-<tab> t s" . perject-tab-cycle-state)
        ("C-<tab> t t" . perject-tab-create)
        ("C-<tab> t T" . perject-tab-delete)
        ("C-<tab> t r" . perject-tab-reset)
        ("C-<tab> t i" . perject-tab-increment-index)
        ("C-<tab> t I" . perject-tab-decrement-index)))

(use-package org-download
  :after org
  :config
  (setopt org-download-image-dir (concat org-directory "/media"))
  (add-hook 'dired-mode-hook 'org-download-enable))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when (schrenker/wsl2-p) (load "~/.config/emacs/secret/work.el" 'noerror 'nomessage))
            (setopt gc-cons-threshold 800000
                    gc-cons-percentage 0.1)))



(provide 'init)
;;; init.el ends here.

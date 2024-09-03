;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This is my personal configuration file I use at three different environment systems - on macOS, WSL2 (wslg) and msys2.

;;; Code:

;; use-package properties order:
;; :ensure
;; :if
;; :after
;; :autoload
;; :commands
;; :hooks
;; :bind[*]
;; :init
;; :config

;;;;;;;;;;;;;; HOOKATION POINT ;;;;;;;;;;;;;;
(use-package emacs
  :ensure nil
  :init
  (setopt gc-cons-threshold (* 1024 1024 100))

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
          display-line-numbers-grow-only t
          display-line-numbers-type 'visual
          electric-pair-open-newline-between-pairs t
          enable-recursive-minibuffers t
          frame-resize-pixelwise t
          frame-title-format '(:eval (concat user-login-name "@" system-name (if buffer-file-truename " :: %f" " :|: [%b]")))
          indent-tabs-mode nil
          inhibit-startup-message t
          inhibit-startup-screen t
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil
          isearch-wrap-pause 'no
          kept-new-versions 6
          kept-old-versions 2
          load-prefer-newer t
          mac-command-modifier 'meta
          mac-option-modifier 'alt
          mac-right-option-modifier nil
          max-lisp-eval-depth 10000
          minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
          mode-line-position-column-line-format '("(%l,%c)")
          native-comp-async-report-warnings-errors nil
          ns-use-proxy-icon (display-graphic-p)
          read-extended-command-predicate #'command-completion-default-include-p
          read-process-output-max (* 1024 1024)
          require-final-newline t ;; POSIX 3.206: Definition of a 'Line'.
          savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
          scroll-conservatively 1000
          scroll-margin 10
          scroll-preserve-screen-position t
          scroll-step 1
          sentence-end-double-space nil
          set-mark-command-repeat-pop t
          switch-to-buffer-obey-display-actions t
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
    (set-frame-font "JetBrainsMono Nerd Font 14" nil t))

  (put 'upcase-region 'disabled nil))

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
  :config
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (or (memq system-type '(darwin gnu/linux)))
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
  (setopt prefix-help-command #'embark-prefix-help-command)
  :config
  (setopt embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
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
         ("M-g f" . consult-flymake)
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

  :init
  (setopt completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))

  :config
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
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("C-SPC" . corfu-insert-separator)
              ("M-n" . nil)
              ("M-p" . nil))
  :init
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
          corfu-max-width 120
          global-corfu-minibuffer nil)
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
  :hook (global-corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-popupinfo-map
              ("C-j" . corfu-popupinfo-scroll-up)
              ("C-k" . corfu-popupinfo-scroll-down))
  :config
  (setopt corfu-popupinfo-delay '(1.0 . 0.5)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :hook (global-corfu-mode . corfu-terminal-mode))

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
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

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
  :bind (("C-x g" . magit-status)
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

  (setopt auto-revert-buffer-list-filter #'magit-auto-revert-repository-buffer-p)

  :config
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1
          magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (display-graphic-p) (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package helpful
  :bind
  (([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable)
   ([remap describe-symbol]   . helpful-symbol))
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
  (advice-add 'describe-key :override 'helpful-key))

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

  :config
  (setopt aw-keys '(?e ?t ?u ?h ?o ?n ?a ?s))

  (when (display-graphic-p)
    (with-eval-after-load 'posframe
      (ace-window-posframe-mode 1))))

(use-package perspective
  :bind
  (("C-x C-b" . schrenker/persp-ibuffer)
   :map perspective-map
   ("B" . nil)
   ("s" . persp-switch-to-scratch-buffer)
   ("C-<tab>" . persp-switch)
   ("TAB" . persp-switch-last))
  :init
  (setopt persp-initial-frame-name "!Main"
          persp-mode-prefix-key (kbd "C-<tab>")
          persp-purge-initial-persp-on-save t
          persp-show-modestring nil
          persp-state-default-file (concat user-emacs-directory "perspfile.el"))
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'elpaca-after-init-hook (lambda () (persp-state-load persp-state-default-file)))

  (defun schrenker/fix-scratch-buffer-default-directory ()
    "Make sure that default-directory for scratch buffers doesn't leak from other perspectives.

This function fetches list of all file-visiting buffers in a perspective, and gets project root from the topmost one of the list. Then it applies this project root to scratch buffer. This ensures, that utilities such as Eat, or Magit start in correct path in a perspective. This approach works, because I usually don't run more than a single vc project in a perspective.

If anything fails, set path to home directory."
    (let* ((realbufs (seq-filter (lambda (buf) (buffer-file-name buf)) (persp-current-buffers)))
           (defdir (or (ignore-errors (with-current-buffer (car realbufs)
                                        (project-root (project-current))))
                       "~/"))
           (scratch (persp-get-scratch-buffer)))
      (with-current-buffer scratch
        (setq-local default-directory defdir))))

  (add-hook 'persp-state-after-load-hook (lambda ()
                                           (add-hook 'persp-switch-hook #'schrenker/fix-scratch-buffer-default-directory)
                                           (add-hook 'project-switch-hook #'schrenker/fix-scratch-buffer-default-directory)
                                           (advice-add 'project-switch-project :after
                                                       (lambda (&rest r) (run-hooks 'project-switch-hook)))))

  (with-eval-after-load 'ibuf-ext
    (define-ibuffer-filter perspective-local-buffers
        "Limit current view to local buffers."
      (:description "local buffers" :reader nil)
      (persp-is-current-buffer buf nil)))

  (with-eval-after-load 'ibuffer
    (require 'ibuf-ext)

    (define-key ibuffer--filter-map (kbd "l")
                #'ibuffer-filter-by-perspective-local-buffers)

    (define-ibuffer-op schrenker/persp-ibuffer-add-to-perspective ()
      "Add marked buffers to current perspective."
      (:opstring "added"
       :active-opstring "add"
       :dangerous t
       :complex t)
      (persp-add-buffer buf))

    (define-ibuffer-op schrenker/persp-ibuffer-remove-from-perspective ()
      "Remove marked buffers from current perspective."
      (:opstring "removed"
       :active-opstring "remove"
       :dangerous t
       :complex t)
      (persp-remove-buffer buf)))

  (defun schrenker/persp-ibuffer (&optional other-window-p noselect shrink)
    "Create dedicated ibuffer instance for current perspective, filtering by current perspective buffers by default."
    (interactive)
    (let ((name (or
                 (seq-find (lambda (b)
                             (string-match-p
                              (concat "*Persp Ibuffer (" (persp-current-name) ")*")
                              (buffer-name b)))
                           (buffer-list))
                 (generate-new-buffer-name (concat "*Persp Ibuffer (" (persp-current-name) ")*")))))
      (ibuffer other-window-p name '((perspective-local-buffers . nil))
               noselect shrink)))

  (persp-mode)

  :config
  (defalias 'persp-feature-flag-prevent-killing-last-buffer-in-perspective #'ignore)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))
  (with-eval-after-load 'consult-project-extra
    (setopt consult-project-extra-sources
            '(consult-project-extra--source-buffer
              consult-project-extra--source-file
              persp-consult-source
              consult-project-extra--source-project))))

(use-package perspective-tabs
  :after perspective
  :ensure (perspective-tabs :host sourcehut :repo "woozong/perspective-tabs")
  :init
  (add-hook 'persp-mode-hook #'perspective-tabs-mode))

(use-package popper
  :after perspective
  :init
  (defun popper-select-popup-at-bottom-maybe-hide (buffer &optional _act)
    "Display popups at the bottom of the screen.
Mark buffer as shown without showing it, if it's supposed to be suppressed."
    (if (popper--suppress-p buffer)
        (display-buffer-no-window buffer '((allow-no-window . t)))
      (popper-select-popup-at-bottom buffer _act)))
  :config
  (setopt popper-group-function #'popper-group-by-perspective
          popper-display-function #'popper-select-popup-at-bottom-maybe-hide
          popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis))
          popper-mode-line-position 1
          popper-reference-buffers
          '(;"\\*Messages\\*"
            "\\*Warnings\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            help-mode
            helpful-mode
            compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package hydra
  :bind (("M-O" . 'schrenker/switch-hydra)
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
          ((eq major-mode 'org-mode) (hydra-org/body))))

  (defhydra hydra-uictl
    (:hint nil)
    "
╭──────────────────────────────────────────────────────────────────^^^^^^
  [_=_] balance    [___] maximize    [_+_] zoom in    [_-_] zoom out
  [_M-k_] vShrink  [_M-j_] vEnlarge  [_M-h_] hShrink  [_M-l_] hEnlarge
 ^^^^^^                                               [_q_] Quit Hydra
 ^^^^^^──────────────────────────────────────────────────────────────────╯
"
    ("=" balance-windows)
    ("_" maximize-window)
    ("+" schrenker/zoom-frame)
    ("-" schrenker/zoom-frame-out)
    ("M-k" shrink-window)
    ("M-j" enlarge-window)
    ("M-h" shrink-window-horizontally)
    ("M-l" enlarge-window-horizontally)
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
  [_c_] Tasks/Completed    [_C_] Tasks/Completed
  [_n_] Notes^^                                    [_q_] Quit Hydra
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
  [_C_] Current Rev^^
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
Notes heading will be sorted by time created in reverse, while Tasks will be sorted by time created.
If no criteria is met, call org-sort."
    (interactive)
    (cond ((string= (org-no-properties (org-get-heading t t t t)) "Notes")
           (org-sort-entries nil ?T))
          ((string= (org-no-properties (org-get-heading t t t t)) "Tasks")
           (org-sort-entries nil ?t))
          ((cl-some (lambda (x) (string= (org-no-properties (org-get-heading t t t t)) x)) '("Backlog" "Active" "Completed"))
           (org-sort-entries nil ?a)
           (org-sort-entries nil ?p)
           (org-sort-entries nil ?o))
          (t
           (call-interactively #'org-sort))))

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
  (setopt org-directory "~/org/"
          org-M-RET-may-split-line '((default . nil))
          org-archive-location (concat org-directory "03_archives/%s_archive::")
          org-archive-tag "archive"
          org-default-notes-file (concat org-directory "inbox.org")
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

(use-package org-reverse-datetree
  :config
  (setq-default org-reverse-datetree-level-formats
                '("%Y W%W"
                  "%Y-%m-%d %A")))

(use-package org-capture
  :ensure nil
  :after org
  :bind (("C-c n n" . org-capture))
  :config
  (setopt org-capture-templates `(("i" "Inbox Note" entry
                                   (file+headline org-default-notes-file "Notes")
                                   ,(schrenker/get-org-template "note")
                                   :empty-lines 1
                                   :prepend nil)
                                  ("I" "Inbox Task" entry
                                   (file+headline org-default-notes-file "Tasks")
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
                                   :prepend t)
                                  ("j" "Journal")
                                  ("j" "Inbox" plain
                                   (file+function org-default-notes-file
                                                  (lambda ()
                                                    (org-reverse-datetree-goto-date-in-file
                                                     nil :olp '("Journal"))))
                                   ""
                                   :tree-type week :unnarrowed t :empty-lines-after 2)))

  (when (schrenker/wsl2-p) (load "~/.config/emacs/secret/work.el" 'noerror 'nomessage)))

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
          org-src-window-setup 'split-window-below
          org-src-preserve-indentation t
          org-babel-shell-names '("bash" "fish" "sh" "zsh" "csh" "ash" "dash" "ksh" "mksh" "posh")
          org-src-lang-modes '(("shell" . bash-ts)
                               ("sh" . bash-ts)
                               ("bash" . bash-ts)
                               ("fish" . fish)
                               ("elisp" . emacs-lisp)
                               ("sqlite" . sql)
                               ("txt" . text)))

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

(use-package corg
  :ensure (:host github :repo "isamert/corg.el")
  :init
  (add-hook 'org-mode-hook #'corg-setup))

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

(use-package org-download
  :after org
  :config
  (setopt org-download-image-dir (concat org-directory "media"))
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-make-toc
  :init
  (setopt org-make-toc-insert-custom-ids nil
          org-make-toc-link-type-fn #'org-make-toc--link-entry-org)
  (add-hook 'org-mode-hook #'org-make-toc-mode))

(use-package embark-org
  :ensure nil
  :after embark org
  :bind (:map embark-org-item-map
              ("RET" . schrenker/org-fullcycle-checkbox)
              :map embark-org-link-map
              ("RET" . org-open-at-point)))

(use-package org-roam
  :autoload (org-roam-db-query)
  :commands (org-roam-capture-p)
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . schrenker/org-roam-node-find-nonarchived)
         ("C-c n F" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n N" . org-roam-capture)
         ("C-c n a" . org-roam-tag-add)
         ("C-c n d" . org-roam-tag-remove)
         ("C-c n u" . schrenker/org-roam-update-tag-collection-nodes)
         ("C-c n s" . org-roam-db-sync))
  :init
  (defun schrenker/agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (interactive)
    (setopt org-agenda-files (seq-uniq
                              (seq-filter
                               (lambda (EL) (not (or (string-match "03_archives" EL)
                                                (string-match "99_tags" EL))))
                               (seq-map
                                #'car
                                (org-roam-db-query
                                 [:select [nodes:file]
                                          :from tags
                                          :left-join nodes
                                          :on (= tags:node-id nodes:id)
                                          :where (or (like tag (quote "%\"agenda\"%"))
                                                     (like tag (quote "%\"project\"%")))]))))))

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
              (tagfile (concat org-directory "99_tags/tag:" tag ".org")))
          (when (file-exists-p tagfile)
            (with-current-buffer (find-file-noselect tagfile)
              (setq-local before-save-hook nil)
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

  (defun schrenker/org-roam-archive-file ()
    "Add archive filetag to org node, and move it to archive directory. Re-visit the buffer, killing the old one."
    (interactive)
    (let ((bf (buffer-file-name)))
      (org-roam-tag-add '("archive"))
      (save-buffer)
      (kill-buffer (current-buffer))
      (rename-file bf (concat org-directory "03_archives/") nil)
      (find-file (concat org-directory "03_archives/" (file-name-nondirectory bf)))))

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
  (setopt schrenker/org-fileslug "${slug}.org"
          org-roam-capture-templates `(("p" "Project")
                                       ("pp" "Minor Project" plain "%?"
                                        :target (file+head
                                                 ,(concat "00_projects/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "project-minor"))
                                        :unnarrowed t)
                                       ("pP" "Major Project" plain "%?"
                                        :target (file+head
                                                 ,(concat "00_projects/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "project-major"))
                                        :unnarrowed t)
                                       ("a" "Area" plain "%?"
                                        :target (file+head
                                                 ,(concat "01_areas/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "area"))
                                        :unnarrowed t)
                                       ("r" "Resource")
                                       ("rr" "Resource" plain "%?"
                                        :target (file+head
                                                 ,(concat "02_resources/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "resource"))
                                        :unnarrowed t)
                                       ("rR" "Runbook" plain "%?"
                                        :target (file+head
                                                 ,(concat "02_resources/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "runbook"))
                                        :unnarrowed t)
                                       ("rc" "Culinary" plain "%?"
                                        :target (file+head
                                                 ,(concat "02_resources/culinary/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "resource-culinary"))
                                        :unnarrowed t)
                                       ("ri" "Investigation" plain "%?"
                                        :target (file+head
                                                 ,(concat "02_resources/" schrenker/org-fileslug)
                                                 ,(schrenker/get-org-template "resource-investigation"))
                                        :unnarrowed t))
          org-roam-directory org-directory
          org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))

  (with-eval-after-load 'org-agenda
    (schrenker/agenda-files-update)
    (advice-add 'org-agenda :before #'schrenker/agenda-files-update)
    (advice-add 'org-todo-list :before #'schrenker/agenda-files-update))

  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after org-roam
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

(use-package ox-icalendar
  :after org
  :ensure nil
  :init
  (defun schrenker/org-icalendar--vevent
      (entry timestamp uid summary location description categories timezone class)
    "Create a VEVENT component just like 'org-icalendar--vevent' does,
but instead of fetching UID from ID property in org, generate one on the fly,
based on file name relative to org directory + heading name.
That way I can avoid org-icalendar-store-UID property completely, and avoid
littering my org mode with ton of PROPERTY drawers under each heading."
    (org-icalendar-fold-string
     (if (eq (org-element-property :type timestamp) 'diary)
         (org-icalendar-transcode-diary-sexp
	      (org-element-property :raw-value timestamp) uid summary)
       (concat "BEGIN:VEVENT\n"
	           (org-icalendar-dtstamp) "\n"
	           "UID:" (md5 (concat (file-relative-name (buffer-file-name) org-directory) "/" (org-element-property :raw-value entry))) "\n"
	           (org-icalendar-convert-timestamp timestamp "DTSTART" nil timezone) "\n"
	           (org-icalendar-convert-timestamp timestamp "DTEND" t timezone) "\n"
	           ;; RRULE.
	           (when (org-element-property :repeater-type timestamp)
	             (format "RRULE:FREQ=%s;INTERVAL=%d\n"
		                 (cl-case (org-element-property :repeater-unit timestamp)
			               (hour "HOURLY") (day "DAILY") (week "WEEKLY")
			               (month "MONTHLY") (year "YEARLY"))
		                 (org-element-property :repeater-value timestamp)))
	           "SUMMARY:" summary "\n"
	           (and (org-string-nw-p location) (format "LOCATION:%s\n" location))
	           (and (org-string-nw-p class) (format "CLASS:%s\n" class))
	           (and (org-string-nw-p description)
		            (format "DESCRIPTION:%s\n" description))
	           "CATEGORIES:" categories "\n"
	           ;; VALARM.
	           (org-icalendar--valarm entry timestamp summary)
	           "END:VEVENT"))))

  (setopt org-icalendar-combined-agenda-file (concat org-directory "emacs.ics")
          org-icalendar-combined-name "Emacs"
          org-icalendar-deadline-summary-prefix ""
          org-icalendar-store-UID nil)

  (advice-add 'org-icalendar--vevent :override #'schrenker/org-icalendar--vevent))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("A" . ibuffer-do-schrenker/persp-ibuffer-add-to-perspective)
         ("K" . ibuffer-do-schrenker/persp-ibuffer-remove-from-perspective)
         ("J" . ibuffer-jump-to-buffer)
         ("M-o" . nil))
  :config
  (setopt ibuffer-eliding-string "…"
          ibuffer-jump-offer-only-visible-buffers t)
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))

(use-package dired
  :ensure nil
  :bind
  (("C-x d" . dired)
   ("C-x C-d" . (lambda () (interactive)(dired default-directory)))
   :map dired-mode-map
   ("_" . dired-create-empty-file)
   ("J" . dired-goto-file)
   ("K" . dired-kill-subdir))
  :init
  ;; aw-flip-window dired workflow start
  (defvar schrenker/last-dired-window-before-jump nil)

  (defun schrenker/dired-find-file-other-window (orig-fun &rest args)
    "Save current window, call ORIG-FUN with ARGS, and add saved window to aw-window-ring.

Purpose of this is to be able to go back to Dired window with aw-flip-window, if Dired window was left by visiting the file in other window.

Additionally, disable dired-preview-mode, if target buffer is dired buffer."
    (setq schrenker/last-dired-window-before-jump (get-buffer-window))
    (let ((result (apply orig-fun args)))
      (when schrenker/last-dired-window-before-jump
        (ring-insert aw--window-ring schrenker/last-dired-window-before-jump))
      (when (derived-mode-p 'dired-mode)
        (dired-preview-mode -1))
      result))

  (advice-add 'dired-find-file-other-window :around #'schrenker/dired-find-file-other-window)
  ;; aw-flip-window dired workflow end

  (add-hook 'dired-mode-hook
            (lambda ()
              (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                                        ace-display-buffer))
                          aw-ignore-current t)
              (auto-revert-mode)
              (display-line-numbers-mode -1)))

  :config
  (setopt dired-listing-switches "-l --almost-all --sort=extension --human-readable --group-directories-first --no-group"
          dired-kill-when-opening-new-dired-buffer t
          dired-dwim-target t)

  (when (eq system-type 'darwin)
    (setopt insert-directory-program "/opt/homebrew/bin/gls")))

(use-package dired-x
  :ensure nil
  :config
  (setopt dired-omit-files (concat dired-omit-files
                                   "\\|^\\.DS_Store"
                                   "\\|^\\.git"
                                   "\\|^\\.stfolder")))

(use-package dired-filter
  :init
  (define-key dired-mode-map (kbd "/") dired-filter-map)
  (add-hook 'dired-mode-hook #'dired-filter-mode))

(use-package dired-preview
  :bind
  (:map dired-mode-map
        ("C-c C-p" . dired-preview-global-mode))
  :init
  (setopt dired-preview-delay 0.1
          dired-preview-max-size (expt 2 20)
          dired-preview-display-action-alist-function (lambda () '((display-buffer-in-previous-window)
                                                              (inhibit-same-window . t)))
          dired-preview-ignored-extensions-regexp
          (concat "\\."
                  "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                  "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                  "\\|iso\\|epub\\|pdf\\)"))
  :config
  (add-to-list 'dired-preview-trigger-commands 'schrenker/meow-next)
  (add-to-list 'dired-preview-trigger-commands 'schrenker/meow-prev)

  (advice-add 'dired-preview--delete-windows :override (lambda () t)))

(use-package dired-narrow
  :bind
  (:map dired-mode-map
        ("C-c f" . dired-narrow)
        ("C-c F" . dired-narrow-fuzzy)))

(use-package dired-collapse
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

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

  (add-hook 'eat-mode-hook (lambda ()
                             (setq-local
                              mode-line-process nil
                              mode-line-buffer-identification (propertized-buffer-identification "%b"))))

  :config
  (setopt eat-kill-buffer-on-exit t
          eat-term-name "xterm-256color")

  (with-eval-after-load 'perspective
    (defun schrenker/eat-project (&optional arg)
      (interactive "P")
      (let* ((default-directory-old default-directory)
             (default-directory (project-root (project-current t)))
             (eat-buffer-name (concat (project-prefixed-buffer-name "eat") " (" (persp-current-name) ")"))
             (default-directory default-directory-old))
        (eat nil arg)))

    (defun schrenker/eat-project-other-window (&optional arg)
      (interactive "P")
      (let* ((default-directory-old default-directory)
             (default-directory (project-root (project-current t)))
             (eat-buffer-name (concat (project-prefixed-buffer-name "eat") " (" (persp-current-name) ")"))
             (default-directory default-directory-old))
        (eat-other-window nil arg)))

    (advice-add 'eat-project :override
                #'schrenker/eat-project)
    (advice-add 'eat-project-other-window :override
                #'schrenker/eat-project-other-window))

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
  (setopt buffer-name-relative-abbrev-limit 20)
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
                                          (python-mode . python-indent-offset)
                                          (t . 2)))
  (add-hook 'yaml-mode-hook #'prism-whitespace-mode)
  (add-hook 'bash-ts-mode-hook #'prism-whitespace-mode)
  (add-hook 'shell-script-mode-hook #'prism-whitespace-mode)
  (add-hook 'python-mode-hook #'prism-whitespace-mode)
  (add-hook 'emacs-lisp-mode-hook #'prism-mode))

(use-package blackout
  :init
  (with-eval-after-load 'format-all
    (blackout 'format-all-mode))
  (with-eval-after-load 'goggles
    (blackout 'goggles-mode))
  (with-eval-after-load 'ws-butler
    (blackout 'ws-butler-mode))
  (with-eval-after-load 'eldoc
    (blackout 'eldoc-mode))
  (with-eval-after-load 'consult-org-roam
    (blackout 'consult-org-roam-mode))
  (with-eval-after-load 'vi-tilde-fringe
    (blackout 'vi-tilde-fringe-mode))
  (with-eval-after-load 'autorevert
    (blackout 'auto-revert-mode))
  (with-eval-after-load 'meow
    (blackout 'meow-beacon-mode)
    (blackout 'meow-keypad-mode)
    (blackout 'meow-motion-mode)
    (blackout 'meow-normal-mode)
    (blackout 'meow-insert-mode))
  (with-eval-after-load 'nerd-icons-dired
    (blackout 'nerd-icons-dired-mode)))

(use-package solaire-mode
  :init
  (solaire-global-mode))

(use-package tab-bar
  :ensure nil
  :init
  (defun schrenker/tab-bar-name (tab i)
    "Add a space on the sides of every tab."
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat "  "
               (if tab-bar-tab-hints (format "%d  " i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               "  ")
       'face (funcall tab-bar-tab-face-function tab))))

  (setopt tab-bar-auto-width nil
          tab-bar-close-button-show nil
          tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
          tab-bar-new-button nil
          tab-bar-new-tab-choice #'scratch-buffer
          tab-bar-separator "\u200B"
          tab-bar-tab-name-format-function #'schrenker/tab-bar-name))

(use-package solarized-theme
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
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (add-to-list 'nerd-icons-extension-icon-alist '("jar"           nerd-icons-devicon "nf-dev-java"       :face nerd-icons-dpurple))
  (add-to-list 'nerd-icons-extension-icon-alist '("jenkinsfile"   nerd-icons-devicon "nf-dev-jenkins"    :face nerd-icons-dpurple))
  (add-to-list 'nerd-icons-extension-icon-alist '("groovy"        nerd-icons-devicon "nf-dev-groovy"     :face nerd-icons-cyan))
  (add-to-list 'nerd-icons-extension-icon-alist '("org_archive"   nerd-icons-sucicon "nf-custom-orgmode" :face nerd-icons-dgreen))
  (add-to-list 'nerd-icons-extension-icon-alist '("jsonnet"       nerd-icons-mdicon  "nf-md-code_json"   :face nerd-icons-blue-alt))
  (add-to-list 'nerd-icons-extension-icon-alist '("libsonnet"     nerd-icons-mdicon  "nf-md-code_json"   :face nerd-icons-blue))

  (add-to-list 'nerd-icons-regexp-icon-alist '("postgresql.conf$" nerd-icons-devicon "nf-dev-postgresql" :face nerd-icons-blue))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^flake.lock$"     nerd-icons-mdicon  "nf-md-nix"         :face nerd-icons-dblue))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^nix.conf$"       nerd-icons-mdicon  "nf-md-nix"         :face nerd-icons-dpink))
  (add-to-list 'nerd-icons-regexp-icon-alist '("Jenkinsfile\\'"   nerd-icons-sucicon "nf-seti-jenkins"   :face nerd-icons-cyan-alt))

  (setcdr (assoc "nf-dev-go" nerd-icons/devicon-alist) "󰟓"))

(use-package nerd-icons-ibuffer
  :config
  (setopt nerd-icons-ibuffer-formats `((mark
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

(use-package nerd-icons-dired
  :init
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

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
            (typst . ("https://github.com/uben0/tree-sitter-typst" "master" "src")))))

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
  :bind ("M-t" . er/expand-region)
  :init
  (setopt er/try-expand-list
          '(er/mark-inside-quotes
            er/mark-outside-quotes
            er/mark-inside-pairs
            er/mark-outside-pairs)))

(use-package surround
  :bind-keymap ("C-t" . surround-keymap))

(use-package avy
  :bind
  (("M-o" . avy-goto-char-timer)
   :map isearch-mode-map
   ("M-o" . avy-isearch)))

(use-package man
  :ensure nil
  :bind (:map Man-mode-map
              ("M-j" . Man-next-section)
              ("M-k" . Man-previous-section)
              ("/" . meow-visit)
              ("n" . schrenker/meow-search)
              ("N" . schrenker/meow-search-backwards)
              (";" . meow-reverse)
              ("J" . Man-goto-section)))

(use-package woman
  :ensure nil
  :bind (:map woman-mode-map
              ("M-j" . Man-next-section)
              ("M-k" . Man-previous-section)
              ("/" . meow-visit)
              ("n" . schrenker/meow-search)
              ("N" . schrenker/meow-search-backwards)
              (";" . meow-reverse)
              ("J" . Man-goto-section)))

(use-package format-all
  :bind
  (("C-c C-f" . format-all-region-or-buffer)
   :map emacs-lisp-mode-map
   ("C-c C-f" . format-all-region-or-buffer))
  :init
  (add-hook 'prog-mode-hook 'format-all-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (add-to-list 'format-all-formatters '("Go" gofmt goimports)))))

(use-package meow
  :config
  (load-file (concat user-emacs-directory "lisp/meovim.el"))
  (global-unset-key (kbd "C-c SPC"))
  (add-to-list 'meow-mode-state-list '(elpaca-ui-mode . motion))
  (add-to-list 'meow-mode-state-list '(dired-mode . motion))
  (add-to-list 'meow-mode-state-list '(ibuffer-mode . motion))
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  (defun schrenker/meow-old-quit ()
    "Quit current window or buffer."
    (interactive)
    (if (> (seq-length (window-list (selected-frame))) 1)
        (delete-window)
      (previous-buffer)))

  (defun schrenker/meow-prefix-define-key (&rest keybinds)
    "DRY function to define common MOTION and NORMAL mode keybinds."
    (apply #'meow-define-keys 'normal keybinds)
    (apply #'meow-define-keys 'motion keybinds))

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
   '("C-M-O" . meow-normal-mode)
   '("<escape>" . nil))

  ;;Disable
  (meow-normal-define-key
   '("e" . ignore)
   '("E" . ignore)
   '("q" . ignore))

  ;;Leader
  (schrenker/meow-prefix-define-key
   '("SPC" . nil)
   '("SPC SPC" . consult-project-extra-find)
   '("S-SPC SPC" . consult-project-extra-find-other-window)
   '("SPC ." . popper-toggle)
   '("SPC ," . popper-cycle)
   '("SPC '" . popper-toggle-type)
   '("SPC <" . previous-buffer)
   '("SPC >" . next-buffer)
   '("SPC u" . winner-undo)
   '("SPC r" . winner-redo)
   '("SPC t" . (lambda () (interactive) (aw-transpose-frame (car (window-list)))))
   '("SPC x" . delete-window)
   '("SPC X" . ace-delete-window)
   '("SPC o" . aw-flip-window)
   '("SPC O" . ace-select-window)
   '("SPC s" . schrenker/ace-swap-window)
   '("SPC +" . hydra-uictl/schrenker/zoom-frame)
   '("SPC -" . hydra-uictl/schrenker/zoom-frame-out)
   '("SPC _" . hydra-uictl/maximize-window)
   '("SPC =" . hydra-uictl/balance-windows)
   '("SPC M-j" . hydra-uictl/enlarge-window)
   '("SPC M-k" . hydra-uictl/shrink-window)
   '("SPC M-h" . hydra-uictl/shrink-window-horizontally)
   '("SPC M-l" . hydra-uictl/enlarge-window-horizontally))

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
          meow-replace-state-name-list '((normal . "N")
                                         (motion . "M")
                                         (keypad . "K")
                                         (insert . "I")
                                         (beacon . "B"))
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

  (meow-global-mode)
  (meow-setup-indicator))


(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure eglot-inlay-hints-mode)
  :bind
  (("C-c c c" . eglot)
   ("C-c c a" . eglot-code-actions)
   ("C-c c r" . eglot-rename)
   ("C-c c i" . eglot-find-implementation)
   ("C-c c d" . eglot-find-declaration)
   ("C-c c t" . eglot-find-typeDefinition))

  :init
  (fset #'jsonrpc--log-event #'ignore)

  (defun schrenker/eglot-capf ()
    (setq-local completion-at-point-functions
                (list #'eglot-completion-at-point
                      #'cape-file
                      #'tempel-expand)))
  (add-hook 'eglot-managed-mode-hook #'schrenker/eglot-capf)

  (setopt eglot-events-buffer-size 0
          eglot-menu-string "󰿘")

  :config
  (add-to-list 'eglot-server-programs '((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                                        . ("gopls" :initializationOptions
                                           (:hints (
                                                    :parameterNames t
                                                    :rangeVariableTypes t
                                                    :functionTypeParameters t
                                                    :assignVariableTypes t
                                                    :compositeLiteralFields t
                                                    :compositeLiteralTypes t
                                                    :constantValues t)))))
  (add-to-list 'eglot-server-programs '(jsonnet-mode . ("jsonnet-language-server"))))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("M-g c" . consult-eglot-symbols)))

(use-package flymake
  :ensure nil
  :hook ((prog-mode text-mode org-mode) . flymake-mode)
  :config
  (setopt flymake-mode-line-lighter "FM"
          flymake-show-diagnostics-at-end-of-line 'short))

(use-package flymake-yamllint
  :init
  (add-hook 'yaml-mode-hook #'flymake-yamllint-setup))

(use-package flymake-golangci
  :ensure (flymake-golangci :host github :repo "storvik/flymake-golangci")
  :init
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (when (derived-mode-p 'go-mode)
                                         (flymake-golangci-load-backend)))))

;;;;;; CONFIGURATION LANGUAGES ;;;;;;
(use-package jsonnet-mode :bind (:map jsonnet-mode-map ("C-c C-f" . format-all-region-or-buffer)))
(use-package yaml-mode)

;;;;;; DOMAIN-SPECIFIC LANGUAGES ;;;;;;
(use-package apparmor-mode)
(use-package bicep-mode :ensure (bicep-mode :host "github.com" :repo "christiaan-janssen/bicep-mode"))
(use-package dockerfile-mode)
(use-package jenkinsfile-mode :mode "\\.jenkinsfile\\'")
(use-package nginx-mode)
(use-package nix-mode)
(use-package prometheus-mode)
(use-package promql-mode :ensure (promql-mode :host "github.com" :repo "Andor/promql-mode"))
(use-package rego-mode)

;;;;;; SHELL ;;;;;;
(use-package fish-mode)

(use-package powershell)
(use-package ob-powershell :after (powershell org))

(use-package sh-script
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (setopt sh-shell "bash")
  (setopt sh-shell-file "/bin/bash")
  :mode
  ("\\.sh\\'" . bash-ts-mode))

;;;;;; GO ;;;;;;
(use-package go-mode
  :init
  (setopt go-mode-indent-offset 4)
  (add-hook 'eglot-managed-mode-hook (lambda () (when (derived-mode-p 'go-mode) (eglot-inlay-hints-mode)))))
(use-package go-gen-test)
(use-package go-tag)


;;;;;; PYTHON ;;;;;;
(use-package python-mode
  :init
  (setopt python-indent-offset 4))

;;;;;; MISC ;;;;;;
(use-package lua-mode)

;;;;;; TEXT ;;;;;;
(use-package markdown-mode)

(provide 'init)
;;; init.el ends here.
;;;;;;;;;;;;;; CURATION POINT ;;;;;;;;;;;;;;

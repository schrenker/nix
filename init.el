;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This is my personal configuration file I use at three different environment systems - on macOS, WSL2 (wslg) and msys2.

;;; Code:

(setq gc-cons-threshold (* 1024 1024 200)
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq-default indent-tabs-mode nil
              truncate-string-ellipsis "…"
              x-stretch-cursor t
              window-combination-resize t
              delete-by-moving-to-trash t
              tab-width 4)

(defvar emacs-appearance 'dark
  "This variable holds initial value for theme if there is no dynamic system in place (macos), or value of theme that has been switched to.")

(defun schrenker/wsl2-p ()
  (and (eq system-type 'gnu/linux)
       (string-match
        "Linux.*Microsoft.*Linux"
        (shell-command-to-string "uname -a"))))

(when (schrenker/wsl2-p)
  (setq
   emacs-appearance 'light
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(setq auto-window-vscroll nil
      backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
      create-lockfiles nil
      custom-file "/dev/null"
      delete-old-versions t
      delete-pair-blink-delay 0
      display-line-numbers-type 'visual
      electric-pair-open-newline-between-pairs t
      frame-resize-pixelwise t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-frame-alist (if (schrenker/wsl2-p) '((top . 1) (left . 1) (width . 120) (height . 40)) '((fullscreen . maximized)))
      initial-major-mode 'org-mode
      initial-scratch-message nil
      kept-new-versions 6
      kept-old-versions 2
      load-prefer-newer t
      mac-command-modifier 'meta
      mac-option-modifier 'alt
      mac-right-option-modifier nil
      max-lisp-eval-depth 10000
      native-comp-async-report-warnings-errors nil
      require-final-newline t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      scroll-conservatively 1000
      scroll-margin 10
      scroll-preserve-screen-position t
      scroll-step 1
      sentence-end-double-space nil
      user-full-name "Sebastian Zawadzki"
      user-mail-address (rot13 "fronfgvna@mnjnqmxv.grpu")
      vc-follow-symlinks nil
      version-control t
      visible-bell (schrenker/wsl2-p)
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(column-number-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-prettify-symbols-mode 1)
(pixel-scroll-mode 1)
(savehist-mode 1)
(winner-mode 1)

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Thanks to Xenodium https://xenodium.com/deleting-from-emacs-sequence-vars
(defun schrenker/remove-from-list-variable ()
  (interactive)
  (let* ((var (intern
               (completing-read "From variable: "
                                (let (symbols)
                                  (mapatoms
                                   (lambda (sym)
                                     (when (and (boundp sym)
                                                (seqp (symbol-value sym)))
                                       (push sym symbols))))
                                  symbols) nil t)))
         (values (mapcar (lambda (item)
                           (setq item (prin1-to-string item))
                           (concat (truncate-string-to-width
                                    (nth 0 (split-string item "\n"))
                                    (window-body-width))
                                   (propertize item 'invisible t)))
                         (symbol-value var)))
         (index (progn
                  (when (seq-empty-p values) (error "Already empty"))
                  (seq-position values (completing-read "Delete: " values nil t)))))
    (unless index (error "Eeek. Something's up."))
    (set var (append (seq-take (symbol-value var) index)
                     (seq-drop (symbol-value var) (1+ index))))
    (message "Deleted: %s" (truncate-string-to-width
                            (seq-elt values index)
                            (- (window-body-width) 9)))))

(defun schrenker/retry-until-success (func max-tries)
  "Run FUNC every second until it returns non-nil or MAX-TRIES is reached."
  (let ((counter 0)
        (timer nil))
    (setq timer
          (run-with-timer
           0 1
           (lambda ()
             (if (or (ignore-errors (funcall func)) (> counter max-tries))
                 (cancel-timer timer)
               (setq counter (1+ counter))))))))

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref "feat/aot-native-comp"
                              :files (:defaults (:exclude "extensions"))
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(add-hook 'elpaca-ui-mode-hook (lambda ()
                                 (bind-key (kbd "/") 'elpaca-ui-search 'elpaca-ui-mode-map)))

(elpaca-wait)

(add-hook 'elpaca-after-init-hook (lambda ()
                                    (when (schrenker/wsl2-p)
                                      (load "~/.config/emacs/secret/work.el" 'noerror 'nomessage))))

(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (or (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(use-package envrc
  :if (or (executable-find "direnv") (eq system-type 'darwin))
  :hook (after-init . envrc-global-mode)
  :init
  (defvar envrc-after-update-hook nil)
  (advice-add #'envrc--update :after (lambda () (run-hooks 'envrc-after-update-hook))))

(defun schrenker/kill-buffer--possibly-save (buffer)
  (let ((response
         (cadr
          (read-multiple-choice
           (format "Buffer %s modified; kill anyway?"
                   (buffer-name))
           '((?y "kill" "kill buffer without saving")
             (?n "cancel" "exit without doing anything")
             (?s "save and then kill" "save the buffer and then kill it")
             (?d "diff" "diff the buffer with original file" ))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((equal response "cancel") nil)
          ((equal response "kill") t)
          ((equal response "diff") (with-current-buffer buffer (diff-buffer-with-file buffer) nil))
          (t (with-current-buffer buffer (save-buffer)) t))))

(advice-add #'kill-buffer--possibly-save :override #'schrenker/kill-buffer--possibly-save)

(defun schrenker/kill-this-buffer ()
  "Kill current buffer without confirmation."
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'schrenker/kill-this-buffer)

(defun schrenker/split-and-follow-horizontally ()
  "Split current window down, and then switch to the newly created window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'schrenker/split-and-follow-horizontally)

(defun schrenker/split-and-follow-vertically ()
  "Split current window right, and then switch to the newly created window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'schrenker/split-and-follow-vertically)

(global-set-key (kbd "<A-backspace>") 'backward-kill-word)

(unbind-key (kbd "M-r"))
(unbind-key (kbd "C-z")) ; suspend emacs
(unbind-key (kbd "C-x C-z")) ; suspend frame
(unbind-key (kbd "<f2>"))
(unbind-key (kbd "<f10>"))

(defalias 'yes-or-no-p 'y-or-n-p)

(if (schrenker/wsl2-p)
    (set-frame-font "JetBrains Mono 10" nil t)
  (set-frame-font "JetBrains Mono 13" nil t))

(defmacro schrenker/call-negative (form)
  "Macro for calling any command with negative argument. FORM in this case is function you want called."
  `(let ((current-prefix-arg -1))
     (call-interactively ,form)))

(use-package hydra
  :bind ("M-o" . 'hydra-uictl/body)
  :init
  (defun schrenker/zoom-frame (&optional amt frame)
    "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
    (interactive "p")
    (let* ((frame (or frame (selected-frame)))
           (font (face-attribute 'default :font frame))
           (size (font-get font :size))
           (amt (or amt 1))
           (new-size (+ size amt)))
      (set-frame-font (font-spec :size new-size) t `(,frame))
      (message "Frame's font new size: %d" new-size)))

  (defun schrenker/zoom-frame-out (&optional amt frame)
    "Call `schrenker/zoom-frame' with negative argument."
    (interactive "p")
    (schrenker/zoom-frame (- (or amt 1)) frame))

  (defun schrenker/aw-flip-window ()
    (interactive)
    (if (and (length< (window-list) 2) (eq system-type 'darwin))
        (shell-command "osascript -e 'tell application \"System Events\" to key down command'; osascript -e 'tell application \"System Events\" to key code 48';osascript -e 'tell application \"System Events\" to key up command'")
      (aw-flip-window)))

  :config
  (setq hydra-is-helpful t)

  (defun schrenker/switch-hydra ()
    (interactive)
    (cond ((and (boundp 'git-timemachine-mode) git-timemachine-mode) (hydra-git-timemachine/body))
          ((eq major-mode 'org-mode) (hydra-org/body))
          (t (hydra-uictl/body))))

  (defhydra hydra-uictl
    (:hint nil)
    "

   ^Movement^^    ^Layout^             ^Sizing^            ^Un/Redo^     ^Popup^          ^Buffer^
╭──────────────────────────────────────────────────────────────────────────────────────────^^^^^^^^^^^^^^^
      ^_K_^        [_o_] flip           [_=_]   balance     [_u_] undo    [_._] show       [_<_] prev
      ^^↑^^        [_O_] select         [_m_]   maximize    [_r_] redo    [_,_] cycle      [_>_] next
  _H_ ←   → _L_    [_s_] swap           [_+_]   zoom in     ^^            [_'_] type       [_b_] buffers
      ^^↓^^        [_2_] split down     [_-_]   zoom out    ^^            [_v_] vTerm      [_B_] ibuffer
      ^_J_^        [_3_] split right    [_M-k_] vShrink     ^^            [_V_] PvTerm     [_f_] findf
     ^^   ^^       [_d_] win delete     [_M-j_] vEnlarge    ^^            [_T_] dired      [_S_] scratch
     ^^   ^^       [_D_] aw delete      [_M-h_] hShrink     ^^^^                           [_Q_] kill
     ^^   ^^       [_X_] single         [_M-l_] hEnlarge    ^^^^
     ^^^^^^^^^^^^
     ^^^^^^^^^^                                                     [_TAB_] Switch Hydra   [_q_] quit
 ^^^^^^^^^^^^^^^──────────────────────────────────────────────────────────────────────────────────────────╯
"
    ("K" windmove-up)
    ("J" windmove-down)
    ("H" windmove-left)
    ("L" windmove-right)
    ("M-k" shrink-window)
    ("M-j" enlarge-window)
    ("M-h" shrink-window-horizontally)
    ("M-l" enlarge-window-horizontally)
    ("o" schrenker/aw-flip-window)
    ("O" ace-select-window)
    ("2" schrenker/split-and-follow-horizontally)
    ("3" schrenker/split-and-follow-vertically)
    ("s" schrenker/ace-swap-window)
    ("d" delete-window)
    ("D" ace-delete-window)
    ("X" delete-other-windows)
    ("=" balance-windows)
    ("m" maximize-window)
    ("+" schrenker/zoom-frame)
    ("-" schrenker/zoom-frame-out)
    ("u" winner-undo)
    ("r" winner-redo)
    ("." popper-toggle-latest)
    ("," popper-cycle)
    ("'" popper-toggle-type)
    ("v" multi-vterm-dedicated-toggle)
    ("V" multi-vterm-project)
    ("T" dirvish-dwim)
    ("<" previous-buffer)
    (">" next-buffer)
    ("b" consult-buffer)
    ("B" ibuffer :color blue)
    ("f" find-file :color blue)
    ("S" scratch-buffer)
    ("Q" schrenker/kill-this-buffer)
    ("TAB" schrenker/switch-hydra :color blue)
    ("q" nil :color blue))

  (with-eval-after-load 'org
    (defhydra hydra-org (:hint nil)
      "

  Refile^                 ^Movement
╭─────────────────────────────────────────────────────────────────^^^^^^
  [_B_] Tasks/Backlog      [_K_] Prev Heading
  [_A_] Tasks/Active       [_J_] Next Heading
  [_C_] Tasks/Completed
  [_TAB_] Uictl
  [_q_] Quit
 ^^^^^^─────────────────────────────────────────────────────────────────╯
"
      ("B" (schrenker/refile (buffer-file-name) "Tasks/Backlog"))
      ("A" (schrenker/refile (buffer-file-name) "Tasks/Active"))
      ("C" (schrenker/refile (buffer-file-name) "Tasks/Completed"))
      ("K" outline-previous-heading)
      ("J" outline-next-heading)
      ("TAB" hydra-uictl/body :color blue)
      ("q" nil :color blue)))

  (with-eval-after-load 'git-timemachine
    (defhydra hydra-git-timemachine (:hint nil)
      "

  ^Rev-Movement       ^Commits^                 ^Misc
╭─────────────────────────────────────────────────────────────────^^^^^^
  [_J_] Next Rev       [_b_] Blame               [_?_] Help
  [_K_] Prev Rev       [_c_] Show Commit         [_S_] Write File
  [_g_] Nth Rev        [_y_] Copy Short Hash     [_q_] Quit Hydra
  [_T_] Fuzzy Rev      [_Y_] Copy Long Hash      [_Q_] Quit Timemachine
  [_C_] Current Rev^^                            [_TAB_] Uictl
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
      ("Q" git-timemachine-quit :color blue))))

(use-package hydra-posframe
  :if (display-graphic-p)
  :elpaca
  (hydra-posframe
   :host "github.com"
   :repo "Ladicle/hydra-posframe")
  :config
  (require 'posframe)
  (setq hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (hydra-posframe-mode 1))

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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t))

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
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("S-SPC" . corfu-insert-separator)
              ("C-S-n" . corfu-move-to-minibuffer)
              :map corfu-popupinfo-map
              ("M-j" . corfu-popupinfo-scroll-up)
              ("M-k" . corfu-popupinfo-scroll-down))

  :config
  (unbind-key (kbd "M-n") 'corfu-map)
  (unbind-key (kbd "M-p") 'corfu-map)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  (setq corfu-auto t
        corfu-auto-prefix 3
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode)
        corfu-cycle t
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

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

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
  ;; :diminish t
  :config
  (setq which-key-sort-order 'which-key-local-then-key-order
        which-key-add-column-padding 3
        which-key-max-display-columns 8
        which-key-show-remaining-keys t)
  (which-key-mode))

(use-package magit
  :init
  (defun schrenker/magit-diff-visit-file-other-window ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively 'magit-diff-visit-file)))
  :bind (("C-x g" . magit-status)
         ("C-x G" . schrenker/magit-status-with-prefix)
         ("C-c g s" . schrenker/smerge-repeatedly)
         :map magit-status-mode-map
         ("o" . schrenker/magit-diff-visit-file-other-window)
         ("K" . magit-discard)
         ("M-j" . magit-section-forward)
         ("M-k" . magit-section-backward)
         ("M-J" . magit-section-forward-sibling)
         ("M-K" . magit-section-backward-sibling)
         ("<escape>" . meow-cancel-selection))
  :config
  (require 'transient)
  (defun schrenker/smerge-repeatedly ()
    "Perform smerge actions again and again"
    (interactive)
    (smerge-mode 1)
    (smerge-transient))
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (schrenker/smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (schrenker/smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (schrenker/smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (schrenker/smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (schrenker/smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (schrenker/smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (schrenker/smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (schrenker/smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (schrenker/smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (schrenker/smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (schrenker/smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (schrenker/smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (schrenker/smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (schrenker/smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (schrenker/smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]])
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (defun schrenker/magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status))))

(use-package with-editor)

(use-package git-timemachine
  :commands (git-timemachine)
  :bind (:map git-timemachine-mode-map
              ("M-O" . hydra-git-timemachine/body)))

(use-package diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (window-system) (diff-hl-margin-mode))
  (global-diff-hl-mode))

(use-package helpful
  :demand t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package prism
  :commands (prism-set-colors prism-whitespace-mode prism-mode)
  :elpaca (prism
           :host "github.com"
           :repo "alphapapa/prism.el")
  :init
  (add-hook 'yaml-mode-hook (lambda () (prism-whitespace-mode 1)))
  (add-hook 'bash-ts-mode-hook (lambda () (prism-whitespace-mode 1)))
  (add-hook 'shell-script-mode-hook (lambda () (prism-whitespace-mode 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (prism-mode 1)))
  :config
  ;; Needed before https://github.com/alphapapa/prism.el/issues/22 is fixed.
  (unless (display-graphic-p)
    (load (concat user-emacs-directory "lisp/prism-cl.el") 'noerror 'nomessage))
  (setq prism-comments nil
        prism-whitespace-mode-indents '((yaml-mode . yaml-indent-offset)
                                        (t . 2))))

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

(use-package consult-project-extra
  :commands (consult-project-extra-find
             consult-project-extra-find-other-window))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-c SPC" . consult-eglot-symbols)))

(use-package consult-org-roam
  :after org-roam
  :bind (("C-c n b" . consult-org-roam-backlinks)
         ("C-c n w" . consult-org-roam-forward-links))
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep
        consult-org-roam-buffer-narrow-key ?r
        consult-org-roam-buffer-enabled nil)
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (defun schrenker/org-cycle-checkbox ()
    (interactive)
    (if (org-at-item-checkbox-p)
        (if (org-list-at-regexp-after-bullet-p "\\(\\[[ ]\\]\\)[ \t]+")
            (org-toggle-checkbox '(16))
          (org-toggle-checkbox))))

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  (with-eval-after-load 'embark-org
    (keymap-set embark-org-item-map "RET" #'schrenker/org-cycle-checkbox))

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

(use-package posframe
  :if (display-graphic-p))

(use-package ace-window
  :config
  (defun schrenker/ace-swap-window ()
    (interactive)
    (let ((aw-ignore-current t))
      (ace-swap-window)))
  
  (setq aw-keys '(?e ?t ?u ?h ?o ?n ?a ?s))
  (when (display-graphic-p)
    (with-eval-after-load 'posframe
      (ace-window-posframe-mode 1))))

(use-package popper
  :init
  (setq popper-display-function
        (defun popper-select-popup-at-bottom-maybe-hide (buffer &optional _act)
          (if (popper--suppress-p buffer)
              (display-buffer-no-window buffer '((allow-no-window . t)))
            (popper-select-popup-at-bottom buffer _act))))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (add-hook 'org-mode-hook
            (lambda () (setq-local popper-reference-buffers (append
                                                        (remove "\\*Warnings\\*" popper-reference-buffers)
                                                        '(("\\*Warnings\\*" . hide))))))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package perject
  :demand t
  :after (savehist popper dirvish)
  :config
  (advice-add 'perject-switch :before (lambda (&rest r) (let ((visible (dirvish-side--session-visible-p)))
                                                     (when (eq visible (selected-window))
                                                       (other-window 1)))))

  (defun schrenker/perject-switch-project-global ()
    "Shows unfiltered list of all collections and projects to switch between them freely"
    (interactive)
    (let ((current-prefix-arg '(4))) ;; emulate C-u
      (call-interactively 'perject-switch)))

  (defun schrenker/perject-switch-collection ()
    (interactive)
    (schrenker/call-negative 'perject-switch))

  (add-to-list 'savehist-additional-variables 'perject--previous-collections)
  ;; Make perject load the collections that were previously open.
  ;; This requires configuring `savehist' (see next code block).
  (setq perject-load-at-startup '("dotfiles")
        perject-save-frames nil
        perject-frame-title-format nil
        perject-switch-to-new-collection t
        perject-save-on-exit 'all
        perject-reload-default '(keep t)
        perject-close-default '(t nil t)
        perject-delete-default '(nil t nil t))

  (setq popper-group-function #'popper-group-by-project)

  (perject-mode 1)
  :bind
  (:map perject-mode-map
        ("C-<tab> C-<tab> c" . perject-create)
        ("C-<tab> C-<tab> r" . perject-rename)
        ("C-<tab> C-<tab> R" . perject-rename-collection)
        ("C-<tab> C-<tab> K" . perject-delete)
        ("C-<tab> C-<tab> e" . perject-open-close-or-reload)
        ("C-<tab> C-<tab> s" . perject-sort)
        ("C-<tab> C-<tab> n" . perject-next-project)
        ("C-<tab> C-<tab> p" . perject-previous-project)
        ("C-<tab> C-<tab> N" . perject-next-collection)
        ("C-<tab> C-<tab> P" . perject-previous-collection)
        ("C-<tab> C-<tab> TAB" . schrenker/perject-switch-project-global)
        ("C-<tab> C-<tab> C-<tab>" . perject-switch)
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
  (setq perject-tab-states '(("mutable" always "⟨" "⟩")
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
  :bind (("C-c n n" . org-capture)
         :map org-mode-map
         ("M-O" . hydra-org/body)
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
         ("C-c C-^" . schrenker/sort-priority-then-state))
  :init
  (defun schrenker/sort-priority-then-state ()
    (interactive)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?p)
    (org-sort-entries nil ?o))

  (defun schrenker/get-org-template (template)
    (with-temp-buffer
      (insert-file-contents (concat user-emacs-directory "templates/" template))
      (buffer-string)))
  (setq time-stamp-active t
        time-stamp-start "#\\+modified: [ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp)
  :config
  (require 'org-crypt)
  (require 'org-agenda)
  (require 'org-capture)
  (load-file (concat user-emacs-directory "lisp/org-format.el"))
  (setf epa-pinentry-mode 'loopback)
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  (setq
   org-log-into-drawer "LOGBOOK"
   org-log-state-notes-insert-after-drawers t
   org-log-done 'time
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-targets `((,(directory-files-recursively org-directory "[a-z0-9]*.org$") :maxlevel . 4))
   org-insert-heading-respect-content t
   org-fontify-whole-heading-line t
   org-tags-exclude-from-inheritance '("crypt"
                                       "verb"
                                       "agenda")
   org-tags-column -77
   org-directory "~/org"
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("1." . "a."))
   org-roam-directory org-directory
   org-archive-location "archive/%s_archive::"
   org-archive-tag "archive"
   org-element-archive-tag "archive"
   org-default-notes-file (concat org-directory "/20221222131538-inbox.org")
   org-crypt-disable-auto-save t
   org-crypt-key (rot13 "fronfgvna@mnjnqmxv.grpu")
   org-priority-highest '?A
   org-priority-lowest  '?D
   org-priority-default '?D
   org-hide-emphasis-markers t
   org-M-RET-may-split-line '((default . nil))
   org-return-follows-link t
   org-agenda-skip-unavailable-files t
   org-fontify-quote-and-verse-blocks t
   org-edit-src-content-indentation 0
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
                        ("txt" . text))
   org-priority-start-cycle-with-default t
   org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "INPROGRESS(i!)" "BLOCKED(b@/!)" "ONHOLD(o@/!)" "REVIEW(r!)" "|" "DELEGATED(e@/@)" "CANCELLED(c@/@)" "DONE(d/@)"))
   org-capture-templates
   `(("p" "Personal Note" entry (file+headline org-default-notes-file "Notes") '(schrenker/get-org-template "note") :empty-lines 1 :prepend t)
     ("P" "Personal Task" entry (file+olp org-default-notes-file "Tasks" "Backlog") '(schrenker/get-org-template "task") :empty-lines 1 :prepend t)
     ("a" "Any Project Note" entry (file+headline (lambda () (schrenker/get-project-file)) "Notes") ,(schrenker/get-org-template "note") :empty-lines 1 :prepend t)
     ("A" "Any Project Task" entry (file+olp (lambda () (schrenker/get-project-file)) "Tasks" "Backlog") ,(schrenker/get-org-template "task") :empty-lines 1 :prepend t)))

  (org-crypt-use-before-save-magic)

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)))
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

  ;;Stolen from https://emacs.stackexchange.com/questions/17282/org-mode-logbook-note-entry-without-logbook-drawer
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

  (defun schrenker/refile (file headline &optional arg)
    (org-refile arg nil (list nil file nil (org-find-olp `(,file ,@(split-string headline "/")) nil))))




  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-format-on-save-mode 1)))
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook (lambda () (save-excursion (when (org-find-dblock "kanban") (org-update-dblock)))) nil t)))
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'schrenker/trim-src-block-buffer nil t))))

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
         ("C-c n u" . schrenker/update-tag-nodes)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))

  :init
  (defun schrenker/get-all-org-tags ()
    (seq-uniq (seq-map #'car
                       (org-roam-db-query [:select [tags:tag] :from tags]))))

  (defun schrenker/get-nodes-by-tag (TAG)
    (org-roam-db-query [:select [nodes:id nodes:title]
                                :from tags
                                :left-join nodes
                                :on (= tags:node-id nodes:id)
                                :where (and (like tags:tag $s1) (not (= nodes:title $s2)))
                                :order-by [(asc title)]]
                       TAG (concat "#" TAG)))

  (defun schrenker/get-project-file ()
    (org-roam-node-file
     (org-roam-node-read nil
                         (lambda (node) (and
                                         (member "project" (org-roam-node-tags node))
                                         (not (string= "#project" (org-roam-node-title node))))))))

  (defun schrenker/update-tag-nodes ()
    (interactive)
    (let ((taglist (schrenker/get-all-org-tags)))
      (dolist (tag taglist)
        (let ((nodes (schrenker/get-nodes-by-tag tag))
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
    (interactive)
    (org-roam-node-find nil nil (lambda (node)
                                  (let ((tags (org-roam-node-tags node)))
                                    (if (eq tags nil)
                                        t
                                      (not (or (member "archive" tags) (member "tag" tags))))))))


  (defun schrenker/agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (interactive)
    (setq org-agenda-files (seq-uniq
                            (seq-map
                             #'car
                             (org-roam-db-query
                              [:select [nodes:file]
                                       :from tags
                                       :left-join nodes
                                       :on (= tags:node-id nodes:id)
                                       :where (like tag (quote "%\"agenda\"%"))])))))

  :config
  (schrenker/agenda-files-update)
  (advice-add 'org-agenda :before #'schrenker/agenda-files-update)
  (advice-add 'org-todo-list :before #'schrenker/agenda-files-update)
  (setq fileslug "%<%Y%m%d%H%M%S>-${slug}.org"
        org-roam-capture-templates `(("p" "Project")
                                     ("pp" "Minor Project" plain "%?"
                                      :target (file+head
                                               ,fileslug
                                               ,(schrenker/get-org-template "project-minor"))
                                      :immediate-finish t :unnarrowed t)
                                     ("pP" "Major Project" plain "%?"
                                      :target (file+head
                                               ,fileslug
                                               ,(schrenker/get-org-template "project-major"))
                                      :immediate-finish t :unnarrowed t)
                                     ("a" "Area" plain "%?"
                                      :target (file+head
                                               ,fileslug
                                               ,(schrenker/get-org-template "area"))
                                      :immediate-finish t :unnarrowed t)
                                     ("r" "Resource")
                                     ("rr" "Resource" plain "%?"
                                      :target (file+head
                                               ,fileslug
                                               ,(schrenker/get-org-template "resource"))
                                      :immediate-finish t :unnarrowed t)
                                     ("ri" "Investigation" plain "%?"
                                      :target (file+head
                                               ,fileslug
                                               ,(schrenker/get-org-template "resource-investigation"))
                                      :immediate-finish t :unnarrowed t))
        org-roam-directory (file-truename "~/org")
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :bind (("C-c n o" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme nil
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  (advice-add 'org-roam-ui-open :after (lambda () (schrenker/retry-until-success #'org-roam-ui-sync-theme 15))))

(use-package org-kanban
  :config
  (setq org-kanban/layout '("…" . 15)))

(use-package org-appear
  :elpaca (org-appear
           :host "github.com"
           :repo "awth13/org-appear")
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-modern
  :config
  (setq org-modern-hide-stars nil
        org-modern-table nil
        org-modern-star nil
        org-modern-checkbox nil
        org-modern-block-fringe nil
        org-modern-list nil)
  (global-org-modern-mode 1))

(use-package german-holidays)

(use-package polish-holidays
  :elpaca (polish-holidays
           :host "github.com"
           :repo "mikolajb/emacs-polish-holidays"
           :main "polish-holidays.el"))

(use-package holidays
  :elpaca nil
  :after (org-agenda polish-holidays)
  :config
  (require 'polish-holidays)
  (require 'german-holidays)
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

(use-package ibuffer
  :elpaca nil
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("J" . ibuffer-jump-to-buffer)
   ("M-o" . nil)))

(use-package dired
  :elpaca nil
  :bind
  (:map dired-mode-map
        ("M-+" . dired-create-empty-file)
        ("J" . dired-goto-file))
  :init
  (setq dired-use-ls-dired t
        dired-dwim-target t)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
        ;; Don't worry, Dirvish is still performant even if you enable all these attributes
        dirvish-attributes '(vc-state subtree-state all-the-icons collapse file-time file-size)
        dirvish-path-separators '("~" "/" "/")
        dirvish-default-layout '(1 0.1 0.5)
        dirvish-layout-recipes '((0 0 0.4) (0 0 0.8) (1 0.08 0.8) (1 0.1 0.5))
        dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")

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
   ("^"   . dired-up-directory)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-T" . dirvish-layout-switch)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


(use-package flymake
  :elpaca nil
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-mode-line-lighter "FM"))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (:map tempel-map
              ("M-j" . tempel-next)
              ("M-k" . tempel-previous))

  :init
  (setq tempel-path (concat user-emacs-directory "templates/tempel"))
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

(use-package mood-line
  :config
  (setq mood-line-show-eol-style t
        mood-line-show-encoding-information t
        mood-line-show-indentation-style t)
  (mood-line-mode 1))

(use-package solarized-theme
  :demand t
  :init
  (setq solarized-use-more-italic t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0
        alacritty-config-path "~/.config/alacritty/alacritty.yml")

  (defun schrenker/set-alacritty-theme (appearance)
    (let ((buffer (find-file-noselect alacritty-config-path)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (if (eq appearance 'light)
            (when (string-prefix-p "#" (thing-at-point 'line t))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
          (unless (string-prefix-p "#" (thing-at-point 'line t))
            (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
        (forward-line 1)
        (if (eq appearance 'light)
            (unless (string-prefix-p "#" (thing-at-point 'line t))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
          (when (string-prefix-p "#" (thing-at-point 'line t))
            (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
        (save-buffer))
      (kill-buffer buffer)))

  (defun schrenker/solarized-theme-overlay (appearance)
    ;; Function that is there just to make my life easier. Reapplies all visual updates, and that's it.
    (let ((bg-main (if (eq appearance 'light) "#fdf6e3" "#002b36"))
          (bg-alt (if (eq appearance 'light) "#eee8d5" "#073642"))
          (bg-highlight (if (eq appearance 'light) "#ece3cc" "#184956"))
          (fg-main (if (eq appearance 'light) "#657b83" "#839496"))
          (fg-alt (if (eq appearance 'light) "#93a1a1" "#586e75"))
          (fg-emph (if (eq appearance 'light) "#586e75" "#93a1a1"))
          (yellow "#b58900")
          (yellow-bg (if (eq appearance 'light) "#f8e8c6" "#273532"))
          (orange "#cb4b16")
          (orange-bg (if (eq appearance 'light) "#fedfc5" "#2b2d2e"))
          (red "#dc322f")
          (red-bg (if (eq appearance 'light) "#ffdec8" "#2d2c31"))
          (magenta "#d33682")
          (magenta-bg (if (eq appearance 'light) "#fdded7" "#272d3c"))
          (violet "#6c71c4")
          (violet-bg (if (eq appearance 'light) "#ebe4e2" "#0c3144"))
          (blue "#268bd2")
          (blue-bg (if (eq appearance 'light) "#e7e8e4" "#003547"))
          (cyan "#2aa198")
          (cyan-bg (if (eq appearance 'light) "#e4ecda" "#013841"))
          (green "#859900")
          (green-bg (if (eq appearance 'light) "#efeac7" "#1d3732"))
          (white "#ffffff")
          (black "#181818"))
      (progn
        (when (eq system-type 'darwin) (schrenker/set-alacritty-theme appearance))
        (setq org-todo-keyword-faces `(("NEXT" :foreground ,yellow :weight bold :inverse-video t)
                                       ("TODO" :foreground ,magenta :weight bold :inverse-video t)
                                       ("INPROGRESS" :foreground ,green :weight bold :inverse-video t)
                                       ("BLOCKED" :foreground ,orange :weight bold :inverse-video t)
                                       ("ONHOLD" :foreground ,cyan :weight bold :inverse-video t)
                                       ("REVIEW" :foreground ,blue :weight bold :inverse-video t)
                                       ("DONE" :foreground ,fg-emph :weight bold :inverse-video t)
                                       ("CANCELLED" :foreground ,fg-alt :weight bold :inverse-video t)
                                       ("DELEGATED"  :foreground ,fg-main :weight bold :inverse-video t))
              org-priority-faces `((?A :foreground ,red :weight bold :inverse-video t)
                                   (?B :foreground ,yellow :weight bold :inverse-video t)
                                   (?C :foreground ,violet :weight bold :inverse-video t)
                                   (?D :foreground ,fg-emph :weight bold :inverse-video t))
              org-modern-todo-faces org-todo-keyword-faces
              org-modern-priority-faces org-priority-faces)
        (setq org-src-block-faces
              `(("emacs-lisp" (:background ,magenta-bg :extend t))
                ("python" (:background ,green-bg :extend t))
                ("yaml" (:background ,cyan-bg :extend t))
                ("json" (:background ,blue-bg :extend t))
                ("bash" (:background ,green-bg :extend t))
                ("sh" (:background ,green-bg :extend t))
                ("shell" (:background ,green-bg :extend t))
                ("fish" (:background ,green-bg :extend t))
                ("nix" (:background ,blue-bg :extend t)))
              org-roam-ui-custom-theme  `((base0 . ,(if (eq appearance 'light) "#FFFBF0" "#073642"))
                                          (base1 . ,(if (eq appearance 'light) "#FCF8ED" "#03282F"))
                                          (base2 . ,(if (eq appearance 'light) "#FCF7E8" "#00212C"))
                                          (base3 . ,(if (eq appearance 'light) "#F2E6CE" "#13383C"))
                                          (base4 . ,(if (eq appearance 'light) "#E1DBCD" "#56697A"))
                                          (base5 . ,(if (eq appearance 'light) "#D6D6D6" "#405A61"))
                                          (base6 . "#96A7A9")
                                          (base7 . "#788484")
                                          (base8 . "#626C6C")
                                          (bg . ,(if (eq appearance 'light) "#FDF6E3" "#002b36"))
                                          (bg-alt . ,(if (eq appearance 'light) "#EEE8D5" "#00212B"))
                                          (fg . ,(if (eq appearance 'light) "#556b72" "#839496"))
                                          (fg-alt . ,(if (eq appearance 'light) "#7B8787" "#657b83"))
                                          (grey . ,(if (eq appearance 'light) "#E1DBCD" "#56697A"))
                                          (blue . "#268bd2")
                                          (cyan . "#2aa198")
                                          (dark-blue . "#3F88AD")
                                          (dark-cyan . "#204052")
                                          (green . "#859900")
                                          (magenta . "#d33682")
                                          (orange . "#cb4b16")
                                          (red . "#dc322f")
                                          (teal . "#35a69c")
                                          (violet . "#6c71c4")
                                          (yellow . "#b58900")))
        (mapc #'disable-theme custom-enabled-themes)
        (pcase appearance
          ('light (load-theme 'solarized-light t))
          ('dark (load-theme 'solarized-dark t)))
        (kind-icon-reset-cache)
        (prism-set-colors
          :num 20
          :desaturations '(0 5 10 15 20)
          :lightens '(0 -15 -30 -45 -60)
          :colors (list blue green cyan yellow))
        (set-face-attribute 'org-level-1 nil :background orange-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-2 nil :background green-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-3 nil :background blue-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-4 nil :background yellow-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-5 nil :background cyan-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-6 nil :background green-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-7 nil :background red-bg :extend t :height 1.1)
        (set-face-attribute 'org-level-8 nil :background blue-bg :extend t :height 1.1)
        (set-face-attribute 'org-modern-todo nil :height 1.0 :weight 'bold :box '(:line-width (1 . 0)))
        (custom-set-faces `(aw-leading-char-face ((t (:inherit org-modern-label :width expanded :weight bold :background ,magenta :foreground ,bg-main :height 3.0 )))))
        (custom-set-faces `(dired-header ((t (:weight bold :background "unspecified" :foreground ,blue)))))
        (custom-set-faces `(org-modern-tag ((t (:inherit (secondary-selection org-modern-label) :weight bold :foreground ,violet :inverse-video t)))))
        (custom-set-faces `(org-modern-statistics ((t (:inherit org-modern-label :weight bold :background ,bg-alt :foreground ,green)))))
        (set-face-background 'org-block bg-alt)
        (set-face-extend 'org-block-begin-line t)
        (set-face-extend 'org-block-end-line t)
        (set-face-attribute 'link nil :foreground cyan :slant 'italic )
        (set-face-attribute 'org-checkbox nil :box `(:line-width (3 . 1) :color ,bg-alt) :background bg-alt)
        (set-face-attribute 'org-modern-date-active nil :foreground fg-emph :background bg-alt)
        (set-face-attribute 'org-modern-date-inactive nil :foreground fg-alt :background bg-alt)
        (set-face-attribute 'org-modern-time-active nil :foreground fg-emph :background bg-main :inverse-video t)
        (set-face-attribute 'org-modern-time-inactive nil :foreground fg-alt :background bg-main :inverse-video t)
        (set-face-foreground 'vc-edited-state yellow)
        (set-face-foreground 'vc-locally-added-state green)
        (set-face-foreground 'vc-removed-state red)
        (set-face-foreground 'vc-missing-state fg-alt)
        (set-face-foreground 'vc-conflict-state orange)
        (set-face-foreground 'vc-locked-state violet)
        (set-face-foreground 'vc-needs-update-state blue)
        (set-face-background 'hl-line bg-alt)
        (require 'embark)
        (set-face-background 'embark-target (if (eq appearance 'light) white black))
        (advice-add
         'org-modern--update-label-face
         :override
         (lambda (&rest r)
           (set-face-attribute 'org-modern-label nil :height 1.0 :box nil)))
        (run-with-idle-timer 0 nil (lambda ()(mapc (lambda (buffer) (with-current-buffer buffer (when (derived-mode-p 'org-mode)(font-lock-update)))) (buffer-list)))))))

  :config
  (defun schrenker/apply-overlay (appearance)
    (setq emacs-appearance appearance)
    (schrenker/solarized-theme-overlay appearance)
    (ignore-errors (org-roam-ui-sync-theme)))
  (cond
   ((not (display-graphic-p))(schrenker/apply-overlay emacs-appearance))
   ((eq system-type 'darwin)(progn
                              (add-hook 'ns-system-appearance-change-functions #'schrenker/apply-overlay)
                              (schrenker/apply-overlay ns-system-appearance)))
   (t (schrenker/apply-overlay emacs-appearance))))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-extension-icon-alist '("jar" all-the-icons-alltheicon "java" :height 1.0 :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-extension-icon-alist '("jenkinsfile" all-the-icons-fileicon "jenkins" :height 1.0 :face all-the-icons-dpurple)))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :init
  (setq all-the-icons-ibuffer-formats `((mark
                                         modified
                                         read-only
                                         ,(if (>= emacs-major-version 26) 'locked "")
                                         " "
                                         (icon 2 2)
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
                                         (name 30 30 :left :elide)
                                         " "
                                         filename-and-process+))))

(use-package all-the-icons-completion
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode 1))

(setq frame-title-format '(:eval (concat user-login-name "@" system-name (if buffer-file-truename " :: %f" " :|: [%b]")))
      ns-use-proxy-icon (display-graphic-p))

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

(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :demand t
  :init
  (setq vterm-always-compile-module t)
  (defun schrenker/CC-out-of-copy-mode ()
    (interactive)
    (meow-normal-mode -1)
    (call-interactively #'schrenker/meow-append-to-end-of-line)
    (vterm-send "C-c"))
  :bind*
  (:map vterm-copy-mode-map
        ("C-c C-c" . schrenker/CC-out-of-copy-mode)
        ("C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-hook 'vterm-mode-hook (lambda ()
                               (meow-normal-mode -1)
                               (add-hook 'meow-normal-mode-hook (lambda () (vterm-copy-mode 1)) nil t)))
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq-local confirm-kill-processes nil)
                               (display-line-numbers-mode -1))))

(use-package multi-vterm
  :if (not (eq system-type 'windows-nt))
  :bind (("C-c t p" . multi-vterm-project)
         ("C-c t t" . multi-vterm-dedicated-toggle)
         ("C-c t T" . multi-vterm-dedicated-select))
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))

(use-package eglot
  :elpaca nil
  :commands (eglot eglot-ensure eglot-inlay-hints-mode)
  :bind
  (("C-c c c" . eglot)
   ("C-c c f" . eglot-format)
   ("C-c c a" . eglot-code-actions)
   ("C-c c r" . eglot-rename)
   ("C-c c i" . eglot-find-implementation)
   ("C-c c d" . eglot-find-declaration)
   ("C-c c t" . eglot-find-typeDefinition))

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

(use-package vundo
  :bind
  ("M-v" . vundo))

(use-package woman
  :elpaca nil
  :bind
  (:map woman-mode-map
        ("M-j" . Man-next-section)
        ("M-k" . Man-previous-section)
        ("/" . meow-visit)
        ("s" . schrenker/meow-search)
        (";" . meow-reverse)
        ("J" . Man-goto-section)))

(use-package info
  :elpaca nil
  :bind
  (:map Info-mode-map
        ("M-j" . Info-next)
        ("M-k" . Info-prev)))

(use-package buffer-name-relative
  :config
  (defun schrenker/buffer-name-relative--abbrev-directory-impl (path overflow)
    "Abbreviate PATH by OVERFLOW characters."
    ;; Skip leading slashes.
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
              ;; The resulting abbreviated name.
              (cons
               ;; The `head'.
               (cond
                ((< 1 len)
                 (concat (substring path 0 (1- beg)) "…"))
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
  (advice-add 'buffer-name-relative--abbrev-directory-impl :override #'schrenker/buffer-name-relative--abbrev-directory-impl)
  (setq buffer-name-relative-abbrev-limit 24)
  (advice-add
   'buffer-name-relative--create-file-buffer-advice
   :before
   (lambda (&rest r)
     (let ((vc-root (buffer-name-relative-root-path-from-vc (nth 1 r))))
       (if vc-root
           (setq buffer-name-relative-prefix
                 (concat "["
                         (let ((vc-dir (file-name-nondirectory (directory-file-name (buffer-name-relative-root-path-from-vc (nth 1 r))))))
                           (if (length> vc-dir 12)
                               (concat (substring vc-dir 0 6) ".." (substring vc-dir -4))
                             vc-dir))
                         "]:"))
         (setq buffer-name-relative-prefix "")))))
  (buffer-name-relative-mode))

(use-package treesit
  :elpaca nil
  :init
  (setq treesit-language-source-alist
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
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))))

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

(use-package format-all
  :init
  (add-hook 'prog-mode 'format-all-mode)
  :config
  (add-hook 'go-ts-mode-hook (lambda ()
                               (add-to-list 'format-all-formatters '("Go" gofmt goimports)))))

(use-package go-mode
  :after eglot
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (setq go-ts-mode-indent-offset 4)
  (add-hook 'go-ts-mode-hook (lambda ()
                               (eglot-inlay-hints-mode 1)
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
                     :constantValues t)))))

(use-package go-eldoc)

(use-package go-guru)

(use-package gorepl-mode)

(use-package go-tag)

(use-package go-gen-test)

(use-package flymake-golangci
  :after go-mode
  :config
  (add-hook 'go-ts-mode-hook (lambda ()
                               (add-hook 'envrc-after-update-hook 'flymake-golangci-load 0 t))))

(use-package python-mode
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (setq python-indent-offset 4)
  (add-hook 'python-ts-mode-hook (lambda ()
                                   (eglot-inlay-hints-mode 1)
                                   (setq-local tab-width 4))))

(use-package json-mode)

(use-package fish-mode)

(use-package fish-completion
  :config
  (global-fish-completion-mode))

(use-package bash-completion)

(use-package sh-script
  :elpaca nil
  :init
  (add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (setq-default sh-shell "bash")
  (setq-default sh-shell-file "/bin/bash")
  :mode
  ("\\.sh\\'" . bash-ts-mode))

(use-package dockerfile-mode)

(use-package jenkinsfile-mode
  :mode "\\.jenkinsfile\\'")

(use-package d2-mode)

(use-package ob-d2)

(use-package verb
  :elpaca
  (verb :files (:defaults "ob-verb.el"))
  :after org
  :bind
  (:map org-mode-map
        ("C-c C-r" . verb-command-map)))

(use-package wgrep)

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

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

  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))
  (when (display-graphic-p)
    (define-key global-map (kbd "M-[") 'insert-pair)
    (define-key global-map (kbd "M-(") 'insert-pair)
    (define-key global-map (kbd "M-{") 'insert-pair)
    (define-key global-map (kbd "M-<") 'insert-pair)
    (define-key global-map (kbd "M-\"") 'insert-pair)
    (define-key global-map (kbd "M-\'") 'insert-pair))


  (define-key global-map (kbd "M-]") (lambda () (interactive) (meow-bounds-of-thing ?s) (delete-pair)))
  (define-key global-map (kbd "M-)") (lambda () (interactive) (meow-bounds-of-thing ?r) (delete-pair)))
  (define-key global-map (kbd "M-}") (lambda () (interactive) (meow-bounds-of-thing ?c) (delete-pair)))
  (define-key global-map (kbd "M->") (lambda () (interactive) (meow-bounds-of-thing ?a) (delete-pair)))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . nil)
   '("SPC" . nil)
   '("SPC SPC" . consult-project-extra-find)
   '("S-SPC S-SPC" . consult-project-extra-find-other-window))

  ;;Disable
  (meow-normal-define-key
   '("e" . ignore)
   '("E" . ignore)
   '("q" . ignore)
   '("SPC" . nil))

  ;;Movement
  (meow-normal-define-key
   '("/" . meow-visit)
   '(":" . meow-goto-line)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("f" . meow-find)
   '("F" . schrenker/meow-find-backwards)
   '("h" . schrenker/meow-left-or-expand)
   '("j" . schrenker/meow-next-or-expand)
   '("k" . schrenker/meow-prev-or-expand)
   '("l" . schrenker/meow-right-or-expand)
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
   '("D" . kill-line)
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
   '("P" . meow-yank)
   '("M-p" . meow-yank-pop)
   '("y" . schrenker/meow-copy))

  ;;Selection
  (meow-normal-define-key
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("%" . meow-block)
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
   '("Q" . schrenker/meow-old-quit)
   '("SPC SPC" . consult-project-extra-find)
   '("S-SPC SPC" . consult-project-extra-find-other-window))

  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (add-hook 'meow-switch-state-hook (lambda (&rest _) (when (symbol-value 'meow-beacon-mode) (corfu-quit))))

  (setq meow-use-clipboard t
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

(elpaca-process-queues)

(provide 'init)
;;; init.el ends here.

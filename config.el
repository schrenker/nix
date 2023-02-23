;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
  (centaur-tabs-init-tabsets-store)
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

(defun fail-silently-advice (func &rest args)
  (ignore-errors
    (apply func args)))

(advice-add 'ace-window-posframe-enable :around #'fail-silently-advice)

(require 'posframe)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "red"
    :weight bold
    :height 2.5))
(after! posframe
(ace-window-posframe-mode 1))

(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))

(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-icon nil
      doom-modeline-buffer-state-icon t)

(setq-default window-combination-resize t)

(setq-default x-stretch-cursor t)

(setq-default truncate-string-ellipsis "…")

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")
(after! persp-mode
  (setq-hook! 'persp-mode-hook uniquify-buffer-name-style 'forward))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(global-subword-mode 1)

(setq-default delete-by-moving-to-trash t)

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

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-project-buffer))

(setq evil-want-Y-yank-to-eol t)

(setq +evil-want-o/O-to-continue-comments nil)

(defun schrenker/evil-change (orig-fn beg end &optional type _ &rest args)
    (apply orig-fn beg end type ?_ args))
(advice-add 'evil-change :around 'schrenker/evil-change)

(setq evil-kill-on-visual-paste nil)

(setq evil-ex-substitute-global t)

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

(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))
(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))

(setq org-startup-folded 'nofold)

(after! org
  (add-hook 'before-save-hook
            (lambda ()
              (unless (and (boundp 'org-capture-mode) org-capture-mode)
                (org-update-all-dblocks)))))

  ;; (add-hook 'org-capture-after-finalize-hook
  ;;           (lambda ()
  ;;             (when (buffer-file-name)
  ;;               (let ((file (buffer-file-name)))
  ;;                 (when (eq (current-buffer) (marker-buffer org-capture-last-stored-marker))
  ;;                   (with-current-buffer (find-file-noselect file)
  ;;                     (org-update-all-dblocks)
  ;;                     (save-buffer))))))))

(map! :map org-mode-map
      :localleader "$" #'org-decrypt-entry
      :localleader "a i" #'org-display-inline-images)

(after! org
  (map! :map org-mode-map
        :nv "gj" #'evil-next-visual-line
        :nv "gk" #'evil-previous-visual-line))

(setq org-directory "/Users/sebastian/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/brain"
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

(require 'projectile)
(require 'magit)

(setq async-shell-command-buffer 'new-buffer)

(defvar brain-sync-last-run nil)

(defun schrenker/synchronize-brain ()
  (when (string-equal (projectile-project-name) "brain")
    (let ((current-time (float-time (current-time))))
      (when (or (not brain-sync-last-run)
                (> (- current-time brain-sync-last-run) 3600))
        (setq brain-sync-last-run current-time)
        (progn
          (message "Synchonizing brain.")
          (magit-call-git "pull" "--autostash" "--rebase")
          (magit-call-git "push")
          (message "Brain synchronized."))))))

(add-hook! 'treemacs-switch-workspace-hook #'schrenker/synchronize-brain)

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
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "BLOCKED(b@/!)" "ONHOLD(o@/!)" "REVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(e@/@)" "CANCELLED(c@/@)"))
   org-todo-keyword-faces
   '(("TODO" :foreground "#8741bb" :weight bold :inverse-video t)
     ("INPROGRESS" :foreground "#98BE65" :weight bold :inverse-video t)
     ("BLOCKED" :foreground "#DA8548" :weight bold :inverse-video t)
     ("ONHOLD" :foreground "#2AA198" :weight bold :inverse-video t)
     ("REVIEW" :foreground "#00BFFF" :weight bold :inverse-video t)
     ("DONE" :foreground "#9FA4BB" :weight bold :inverse-video t )
     ("CANCELLED" :foreground "#574C58" :weight bold :inverse-video t)
     ("DELEGATED"  :foreground "#6c71c4" :weight bold :inverse-video t))))

(setq org-log-into-drawer "LOGBOOK")

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

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "-") ("1." . "a.")))

(after! org-fancy-priorities
  (setq
   org-fancy-priorities-list '((65 . "⁂")
                               (66 . "⁑")
                               (67 . "⁕"))))

(after! org
  (setq org-tags-column -77))

(add-hook 'org-mode-hook #'+word-wrap-mode)

(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-hide-emphasis-markers t)

(require 'polish-holidays)
(require 'german-holidays)

  (use-package! holidays
    :after org-agenda
    :config
    (setq calendar-holidays
          (append '((holiday-fixed 1 1 "New Year's Day")
                    (holiday-fixed 2 14 "Valentine's Day")
                    (holiday-fixed 4 1 "April Fools' Day")
                    (holiday-fixed 10 31 "Halloween")
                    (holiday-easter-etc)
                    (holiday-fixed 12 25 "Christmas")
                    (solar-equinoxes-solstices))
                  ustawowo-wolne-od-pracy
                  czas-letni
                  swieta-panstwowe-pozostałe-święta
                  holiday-german-holidays)))

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
          ("t" "Task" entry (file+olp org-default-notes-file "Tasks" "Backlog")
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

(setq +treemacs-git-mode 'deferred)

(set-popup-rule! "^ \\*Treemacs-Scoped-Buffer-Perspective [^*]*\\*" :ignore t)

(setq treemacs-follow-mode t)

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(setq doom-themes-treemacs-theme "doom-colors")

(setq corfu-preview-current 'insert
      corfu-preselect 'prompt ;; Disable candidate preselection
      corfu-on-exact-match nil
      corfu-separator ?\s
      corfu-excluded-modes
      '(erc-mode
        circe-mode
        help-mode
        gud-mode
        vterm-mode))

    (map! ;;:desc "complete" "TAB" #'completion-at-point
     :map corfu-map
     :desc "next" "TAB" #'corfu-next
     :desc "next" "<tab>" #'corfu-next
     :desc "next" [tab] #'corfu-next
     :desc "previous" "S-TAB" #'corfu-previous
     :desc "previous" "<backtab>"  #'corfu-previous
     :desc "previous" [backtab] #'corfu-previous)

(global-corfu-mode)

(setq +lsp-company-backends nil
      +vertico-company-completion-styles nil)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))
      orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  orderless-prefixes
                                  orderless-initialism)
      orderless-component-separator ?\s)

(setq vterm-always-compile-module t)

(setq vterm-max-scrollback 100000)

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

(setq x509-openssl-cmd "/opt/homebrew/Cellar/openssl@3/3.0.5/bin/openssl" )

(fset 'rainbow-delimiters-mode #'prism-mode)

(defun schrenker/prism-set-colors ()
    (prism-set-colors
      :desaturations '(0) ; do not change---may lower the contrast ratio
      :lightens '(0)      ; same
      :colors (list
               (doom-color 'blue)
               (doom-color 'violet)
               (doom-color 'yellow)
               (doom-color 'cyan)
               (doom-color 'green)))
    (message "Prism colors set."))

(use-package! prism
  :commands prism-mode
  :init
  (add-hook! '(json-mode-hook prog-mode-hook)
    (unless (derived-mode-p 'sh-mode 'python-mode)
      (prism-mode 1)))

  ;; (add-hook! '(yaml-mode-hook sh-mode-hook python-mode-hook fish-mode-hook)
  ;;   (prism-whitespace-mode 1))



  :config
  (after! doom-themes
    (setq prism-comments nil)
     ;; prism-num-faces 5
     ;; prism-whitespace-mode-indents '((python-mode . python-indent-offset)
     ;;                                 (haskell-mode . haskell-indentation-left-offset)
     ;;                                 (yaml-mode . yaml-indent)
     ;;                                 (sh-mode . sh-basic-offset)
     ;;                                 (t . 4)))
    (schrenker/prism-set-colors)
    (add-hook! '(prog-mode-hook doom-load-theme-hook ns-system-appearance-change-functions) (schrenker/prism-set-colors))))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write
  vlf-search vlf-occur vlf-follow vlf-ediff vlf
  :commands vlf vlf-mode
  :init
  (defadvice! +files--ask-about-large-file-vlf (size op-type filename offer-raw)
  "Like `files--ask-user-about-large-file', but with support for `vlf'."
  :override #'files--ask-user-about-large-file
  (if (eq vlf-application 'dont-ask)
      (progn (vlf filename) (error ""))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) op-type)))
      (if (not offer-raw)
          (if (y-or-n-p prompt) nil 'abort)
        (let ((choice
               (car
                (read-multiple-choice
                 prompt '((?y "yes")
                          (?n "no")
                          (?l "literally")
                          (?v "vlf"))
                 (files--ask-user-about-large-file-help-text
                  op-type (funcall byte-count-to-string-function size))))))
          (cond ((eq choice ?y) nil)
                ((eq choice ?l) 'raw)
                ((eq choice ?v)
                 (vlf filename)
                 (error ""))
                (t 'abort)))))))
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local +vlf-cumulative-linenum '((0 . 0))
  "An alist keeping track of the cumulative line number.")

(defun +vlf-update-linum ()
  "Update the line number offset."
  (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            +vlf-cumulative-linenum))))

(add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

;; Since this only works with absolute line numbers, let's make sure we use them.
(add-hook! 'vlf-mode-hook (setq-local display-line-numbers t))

(defun +vlf-next-chunk-or-start ()
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))

(defun +vlf-last-chunk-or-end ()
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))

(defun +vlf-isearch-wrap ()
  (if isearch-forward
      (+vlf-next-chunk-or-start)
    (+vlf-last-chunk-or-end)))

(add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'+vlf-isearch-wrap)))

(unless IS-MAC
  ;;Start emacs non-maximized
  (setq initial-frame-alist '((top . 1) (left . 1) (width . 120) (height . 40)))
  ;;Unset problematic keybinds
  (map! "M-m" nil))

(load "~/.config/doom/work.el" t t)

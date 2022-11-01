;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +tng)                            ; the ultimate code completion backend
       (vertico +icons)

       :ui
       doom                                      ; what makes DOOM look the way it does
       doom-dashboard                            ; a nifty splash screen for Emacs
       doom-quit                                 ; DOOM quit-message prompts when you quit Emacs
       indent-guides                             ; highlighted indent columns
       modeline                                  ; snazzy, Atom-inspired modeline, plus API
       ophints                                   ; highlight the region an operation acts on
       (popup +defaults +all)                    ; tame sudden yet inevitable temporary windows
       treemacs                                  ; a project drawer, like neotree but cooler
       unicode                                   ; extended unicode support for various languages
       (vc-gutter +diff-hl +pretty)                                 ; vcs diff in the fringe
       vi-tilde-fringe                           ; fringe tildes to mark beyond EOB
       (window-select +numbers)                  ; visually switch windows
       workspaces                                ; tab emulation, persistence & separate workspaces
       tabs

       :editor
       (evil +everywhere)                        ; come to the dark side, we have cookies
       file-templates                            ; auto-snippets for empty files
       fold                                      ; (nigh) universal code folding
       (format +onsave)                          ; automated prettiness
       snippets                                  ; my elves. They type so I don't have to
       word-wrap

       :emacs
       ( dired +icons +ranger)                  ; making dired pretty [functional] directory editor
       electric                                  ; smarter, keyword-based electric-indent
       ( ibuffer +icons )                        ; interactive buffer management
       ( undo +tree )                            ; persistent, smarter undo for your inevitable mistakes
       vc                                        ; version-control and Emacs, sitting in a tree

       :term
       vterm                                     ; the best terminal emulation in Emacs

       :checkers
       syntax                                    ; tasing you for every semicolon you forget
       (spell +flyspell)

       :tools
       ansible
       (debugger +lsp)
       direnv
       (docker +lsp)
       (eval +overlay)                           ; run code, run (also, repls)
       lookup                                    ; navigate your code and its documentation
       (lsp +peek)                               ; M-x vscode
       magit                                     ; a git porcelain for Emacs
       make                                      ; run make tasks from Emacs
       pdf                                       ; pdf enhancements
       rgb                                       ; creating color strings
       terraform                                 ; infrastructure as code
       upload                                    ; map local to remote projects via ssh/ftp
       tree-sitter

       :os
       (:if IS-MAC macos)                        ; improve compatibility with macOS
       ( tty +osc)                               ; improve the terminal Emacs experience

       :lang
       (go +lsp)
       (python +lsp +pyright +pyenv)             ; beautiful is better than ugly
       (org +pretty +pomodoro +dragndrop +roam2) ; organize your plain life in plain text
       (sh +lsp +fish +powershell)               ; she sells {ba,z,fi}sh shells on the C xor
       json                                      ; At least it ain't XML
       yaml                                      ; JSON, but readable
       emacs-lisp                                ; drown in parentheses
       rest
       (nix +lsp)

       :app
       calendar

       :config
       literate
       (default +bindings +smartparens))

(after! comp
  (mapc (doom-partial #'add-to-list 'native-comp-deferred-compilation-deny-list)
        (list "/emacs-jupyter.*\\.el\\'"
              "/evil-collection-vterm\\.el\\'"
              "/vterm\\.el\\'"
              "/with-editor\\.el\\'")))

(setq native-comp-deferred-compilation nil)
(after! (doom-packages straight)
  (setq straight--native-comp-available t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

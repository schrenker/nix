#!/usr/bin/env fish

if status is-interactive
    fish_vi_key_bindings
end

fish_add_path /opt/homebrew/sbin
fish_add_path /opt/homebrew/bin
fish_add_path ~/.local/go/bin
fish_add_path ~/.config/emacs

set -gx LESSHISTFILE -
set -gx GOPATH ~/.local/go

# pyenv init - | source

# status --is-interactive; and source (pyenv virtualenv-init -|psub)

# integration/fish --- Fish integration

# Copyright (C) 2022-2024 Akib Azmain Turja. Edits made by Sebastian Zawadzki.

# This file is not part of GNU Emacs.

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# For a full copy of the GNU General Public License
# see <https://www.gnu.org/licenses/>.


# Features missing compared to the bash/zsh integrations:
# - Fish history import into Emacs.
# - The prompt markers are not portable, they depend on a specific prompt customization.
# - The PS2 prompt support (line continuation).

function eat_enable_integration
    set -g __eat_integration_enabled yes

    function __eat_chpwd --on-variable PWD
        # Send the current working directory, for directory tracking.
        printf '\e]51;e;A;%s;%s\e\\\\'         \
            "$(echo -n -- $hostname | base64)" \
            "$(echo -n -- $PWD | base64)"
    end

    function __eat_preexec --on-event fish_preexec
        set current_command $argv[1]
        # Send current command.
        printf '\e]51;e;F;%s\e\\\\' \
            "$(echo -n -- $current_command | base64)"

        # Send pre-exec sequence.
        printf '\e]51;e;G\e\\\\'

        # Update title to include the command running.
        # "${PWD/$HOME/'~'}" converts "/home/akib/foo/" to "~/foo/".
        # The next one is substituted with '$', or '#' if we're "root".
        printf '\e]2;%s@%s:%s%s\e\\\\' "$USER" "$hostname" \
            "$(string replace $HOME '~' $PWD)"             \
            "$(fish_is_root_user && echo '#' || echo '$')" \
            "$current_command"
    end

    function __eat_postexec --on-event fish_postexec
        set exit_status $status
        # Send exit status.
        printf '\e]51;e;H;%i\e\\\\' $exit_status

        # Inform that a new prompt is going to be printed.
        printf '\e]51;e;J\e\\\\'

        # Update title.
        # "${PWD/$HOME/'~'}" converts "/home/akib/org/" to "~/org/".
        # The next one is substituted with '$', or '#' if we're "root".
        printf '\e]2;%s@%s:%s%s\e\\\\' "$USER" "$hostname" \
            "$(string replace $HOME '~' $PWD)"             \
            "$(fish_is_root_user && echo '#' || echo '$')"
    end

    # FIXME: These are custom variables for a modified prompt, not native to Fish.
    set --global --prepend fish_before_prompt \
        (printf '\e]51;e;B\e\\\\')
    set --global --append  fish_after_prompt  \
        (printf '\e]51;e;C\e\\\\')
end

function _eat_msg
    set msg (printf '\e]51;e;M')
    for arg in $argv
        set msg $msg "$(echo -n -- $arg | base64)"
    end
    printf "%s\e\\\\" (string join ";" $msg)
end

if status is-interactive
    and test -z $__eat_integration_enabled
    and set -q EAT_SHELL_INTEGRATION_DIR
    and test -n "$INSIDE_EMACS"

    eat_enable_integration
end

# Local Variables:
# mode: fish
# End:

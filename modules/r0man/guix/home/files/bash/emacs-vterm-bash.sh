# Some of the most useful features in emacs-libvterm require shell-side
# configurations. The main goal of these additional functions is to enable the
# shell to send information to `vterm` via properly escaped sequences. A
# function that helps in this task, `vterm_printf`, is defined below.
#
# This is a patched copy of emacs-vterm's etc/emacs-vterm-bash.sh. Deviation
# from upstream: the OSC prompt and title integration (PROMPT_COMMAND title
# echo, vterm_prompt_end appended to PS1) is emitted only when running inside
# vterm ($INSIDE_EMACS contains "vterm"). Upstream appends vterm_prompt_end to
# PS1 unconditionally, so every interactive bash — including the login shell
# TRAMP starts when connecting via /ssh:… — ends its prompt with an OSC 51;A
# sequence *after* the prompt character. TRAMP's shell-prompt regexp must match
# up to end of buffer, so the trailing OSC makes TRAMP spin forever looking for
# a prompt it can never see ("Opening connection for … using ssh…" hangs, the
# Emacs process burns a core). Plain ssh sessions that aren't Emacs don't need
# the integration either.

function vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# With vterm_cmd you can execute Emacs commands directly from the shell.
# For example, vterm_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# vterm-eval-cmds.
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# The shell-side vterm integration only makes sense inside vterm: outside of
# it nobody consumes the OSC sequences, and the trailing OSC in the prompt
# breaks TRAMP's shell-prompt detection (see the header comment above).
if [[ "$INSIDE_EMACS" == *vterm* ]]; then
    # This is to change the title of the buffer based on information provided
    # by the shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the
    # meaning of the various symbols.
    PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'

    # Sync directory and host in the shell with Emacs's current directory.
    # You may need to manually specify the hostname instead of $(hostname) in
    # case $(hostname) does not return the correct string to connect to the
    # server.
    #
    # The escape sequence "51;A" has also the role of identifying the end of
    # the prompt
    vterm_prompt_end(){
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi

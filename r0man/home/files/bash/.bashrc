# .BASH_PROFILE VS .BASHRC
# http://www.joshstaiger.org/archives/2005/07/bash_profile_vs.html

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

# Load bash aliases.
[ -r ~/.bash_aliases ] && . ~/.bash_aliases

# Autojump
[ -r /etc/profile.d/autojump.bash ] && . /etc/profile.d/autojump.bash

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion

# BASH
shopt -s histappend

## FZF

[ -r /usr/share/fzf/key-bindings.bash ] && source /usr/share/fzf/key-bindings.bash
[ -r /usr/share/fzf/completion.bash ] && source /usr/share/fzf/completion.bash

# Fix <RET> in Emacs, see: https://github.com/akermu/emacs-libvterm/issues/40
export FZF_DEFAULT_OPTS="--bind=ctrl-j:accept"

## LIBVTERM

function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# Enable directory tracking: https://github.com/akermu/emacs-libvterm#directory-tracking
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;
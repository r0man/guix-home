# .BASH_PROFILE VS .BASHRC
# http://www.joshstaiger.org/archives/2005/07/bash_profile_vs.html

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Keychain
eval $(keychain --eval --agents "gpg,ssh" --quiet id_rsa)

# Add ssh keys.
for i in $(ls -1 ~/.ssh/*.pub ); do
    ssh-add ${i/.pub/}
done

# SOLARIZED

export SOLARIZED="dark"
if [ -e ~/.dotfiles/dircolors.ansi-$SOLARIZED ]; then
    eval `dircolors ~/.dotfiles/dircolors.ansi-$SOLARIZED`
fi

# CONFLUENT PLATFORM

export CONFLUENT_HOME="$HOME/local/confluent"
export PATH="$CONFLUENT_HOME/bin:$PATH"

# Coursier
export PATH="$PATH:/home/roman/.local/share/coursier/bin"

## Flutter
export CHROME_EXECUTABLE="chromium"
export PATH="$PATH:$HOME/local/flutter/bin"

## High DPI
export GDK_SCALE=1.5
export GDK_DPI_SCALE=1.5

## NPM
NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$PATH:$NPM_PACKAGES/bin"

## MAN

# Preserve MANPATH if you already defined it somewhere in your config.
# Otherwise, fall back to `manpath` so we can inherit from `/etc/manpath`.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

## KAFKA

KAFKA_HOME="$HOME/local/kafka"
if [ -d $KAFKA_HOME ]; then
    export KAFKA_HOME
    export PATH="$KAFKA_HOME/bin:$PATH"
fi

# GOOGLE

# See: https://developers.google.com/identity/protocols/application-default-credentials
#export GOOGLE_APPLICATION_CREDENTIALS="$HOME/.config/gcloud/application_default_credentials.json"

## NUBANK
[ -r /home/roman/.nurc ] && source /home/roman/.nurc
export NU_COUNTRY="br"

## NUCLI.PY
export PATH="$NU_HOME/nucli.py/bin:$PATH"
export NUCLI_PY_FORMATTER_STYLE="bw"
# export NUCLI_PY_FULL=1

# POSTGRESQL OPERATOR

export PATH="${HOME?}/.pgo/pgo:$PATH"

export PGOUSER="${HOME?}/.pgo/pgo/pgouser"
export PGO_APISERVER_URL='https://127.0.0.1:8443'
export PGO_CA_CERT="${HOME?}/.pgo/pgo/client.crt"
export PGO_CLIENT_CERT="${HOME?}/.pgo/pgo/client.crt"
export PGO_CLIENT_KEY="${HOME?}/.pgo/pgo/client.key"
export PGO_NAMESPACE=pgo

# SPARK
export SPARK_HOME="$HOME/workspace/nu/spark/2.4.3"

# V8
export V8_HOME="/usr/bin"

# PATH
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"
export PATH="$HOME/.dotfiles/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.node_modules/bin:$PATH"
export PATH="$HOME/bin:$PATH"

export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

# opam configuration
test -r /home/roman/.opam/opam-init/init.sh && . /home/roman/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
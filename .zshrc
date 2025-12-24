# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    colored-man-pages
    colorize
    docker
    docker-compose
    # emacs
    git
    kubectl
    macos
    npm
    python
    zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

#export PATH=$HOME/.rbenv/bin:/opt/chefdk/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:bin:$PATH:/usr/bin:/bin:/usr/sbin:/sbin

export PATH=$PATH:/Users/udbhav/.linkerd2/bin

export EDITOR=emacsclient
# rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm

# default env
if [ -f "/Users/udbhav/.default.env" ]; then
    source "/Users/udbhav/.default.env"
fi

# mac emacs
# alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias kcat_prod="kcat -F ~/.config/kcat_prod.conf -C -s avro -r \$IC_SCHEMA_REGISTRY_URL"
alias kcat_local="kcat -C -s avro -r localhost:8081"
alias kc="kubectl"

# emacs vterm
# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# clear for emacs vterm
# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# vterm support
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"

. "$HOME/.local/bin/env"

# alias claude="/Users/udbhav/.claude/local/claude"

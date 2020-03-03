# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH=/Users/arnarthor/.oh-my-zsh

ZSH_THEME="powerlevel10k/powerlevel10k"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    kubectl
)

# User configuration

export PATH="node_modules/.bin:/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:/Users/arnarthor/.opam/4.02.3/bin:/Users/arnarthor/.cargo/bin:/Applications/Postgres.app/Contents/Versions/latest/bin"
# export MANPATH="/usr/local/man:$MANPATH"
source $ZSH/oh-my-zsh.sh
ulimit -n 4000

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
export EDITOR='nvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

fpath=(~/.zsh/completions $fpath)
autoload -U compinit && compinit

# OPAM configuration
. /Users/arnarthor/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
export PATH=$PATH:/Users/arnarthor/Library/Android/sdk/platform-tools:/usr/local/Cellar/node/12.6.0/bin
defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
defaults write -g ApplePressAndHoldEnabled -bool false
alias oni='/Applications/Onivim2.app/Contents/MacOS/Oni2'
alias vim='nvim'


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
launchctl setenv PATH $PATH
eval "$(fnm env --multi)"

source "/usr/local/opt/kube-ps1/share/kube-ps1.sh"
PS1='$(kube_ps1)'$PS1

set_aws_profile() {
    show_usage() {
        echo "Syntax: set_aws_profile <environment>"
    }
    if [[ "$#" != 1 ]] ; then
        show_usage
    fi
    if [[ "$1" =~ ^play-dev$\|^play-prod$\|^avilabs-dev$ ]] ; then
        export AWS_REGION=eu-west-1
        export AWS_DEFAULT_REGION=eu-west-1
        export AWS_PROFILE="$1"
    else
        show_usage
    fi
}

get_aws_profile() {
    show_usage() {
        echo "Syntax: get_aws_profile"
    }
    if [[ "$#" != 0 ]] ; then
        show_usage
    fi
    if [[ -n "$AWS_PROFILE" ]] ; then
        profile="$AWS_PROFILE"
    else
        profile=none
    fi
    echo "(aws: ${profile})"
}
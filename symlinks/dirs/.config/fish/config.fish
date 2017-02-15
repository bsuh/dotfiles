# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme robbyrussell

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

[ -f /usr/local/share/autojump/autojump.fish ]; and . /usr/local/share/autojump/autojump.fish

set -gx PATH ~/mybins ~/mybins/depot_tools /usr/local/sbin $HOME/go/bin $PATH
set -gx KONSOLE_DBUS_SESSION 1
set -gx EDITOR emacs
set -gx VISUAL emacs
set -gx GOPATH $HOME/go

function nuget
  mono ~/mybins/nuget.exe $argv
end
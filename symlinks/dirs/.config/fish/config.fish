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

set -gx PATH ~/mybins ~/animeget $PATH
set -gx KONSOLE_DBUS_SESSION 1
set -gx EDITOR emacs
set -gx VISUAL emacs

function nuget
  mono ~/mybins/nuget.exe $argv
end

function livestreamer
  command livestreamer -p mpv --http-header "User-Agent=Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_2 like Mac OS X) AppleWebKit/600.1.4 (KHTML, like Gecko) Version/8.0 Mobile/12A405 Safari/600.1.4" $argv
end

function twitch
  livestreamer "http://www.twitch.tv/"$argv[1] $argv[2..-1]
end

function peerflix
  command peerflix --mpv --not-on-top $argv
end

function emetric-update
  for folder in (ls)
    if test -d $folder/.git
      echo $folder
      git -C $folder stash
      git -C $folder svn rebase
      git -C $folder stash pop
    end
  end
end
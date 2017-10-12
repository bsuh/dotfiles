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

set -gx PATH ~/bin ~/bin/depot_tools $HOME/go/bin $PATH
#set -gx KONSOLE_DBUS_SESSION 1
set -gx EDITOR emacs
set -gx VISUAL emacs
set -gx GOPATH $HOME/go

function nuget
  mono ~/mybins/nuget.exe $argv
end

function tether1
  sudo badvpn-tun2socks --tundev utun0 --netif-ipaddr 10.0.0.2 --netif-netmask 255.255.255.0 --socks-server-addr 172.20.10.1:3128
end

function tether2
  sudo ipconfig set utun0 MANUAL 10.0.0.1 255.255.255.0
  sudo route add 0.0.0.0/1 10.0.0.2 -ifp utun0
  sudo route add 128.0.0.0/1 10.0.0.2 -ifp utun0
  sudo route add 66.162.88.82 172.20.10.1
end

function vpn_routes
  sudo route add 192.168.60.0/24 -iface ppp0
  sudo route add 192.168.62.0/24 -iface ppp0
  sudo route add 192.168.63.0/24 -iface ppp0
  sudo route add 66.162.88.83 -iface ppp0
end
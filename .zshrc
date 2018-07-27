# prompt
export PS1='%{%F{red}%}%~%{%f%} %# '

# color ls
alias ls='gls --color=auto'

# j
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# option-right , option-left shortcut
bindkey "^[^[[C" forward-word
bindkey "^[^[[D" backward-word

export PATH=~/bin:~/bin/depot_tools:~/go/bin:/usr/local/bin:$PATH
export KONSOLE_DBUS_SESSION=1
export EDITOR=emacs
export VISUAL=emacs
export GOPATH=~/go

alias nuget='mono ~/bin/nuget.exe'

function tether1 {
  sudo badvpn-tun2socks --tundev utun0 --netif-ipaddr 10.0.0.2 --netif-netmask 255.255.255.0 --socks-server-addr 172.20.10.1:3128
}

function tether2 {
  sudo ipconfig set utun0 MANUAL 10.0.0.1 255.255.255.0
  sudo route add 0.0.0.0/1 10.0.0.2 -ifp utun0
  sudo route add 128.0.0.0/1 10.0.0.2 -ifp utun0
  sudo route add 66.162.88.82 172.20.10.1
}

function vpn_routes {
  sudo route add 192.168.60.0/24 -iface ppp0
  sudo route add 192.168.62.0/24 -iface ppp0
  sudo route add 192.168.63.0/24 -iface ppp0
  sudo route add 192.168.70.0/24 -iface ppp0
  sudo route add 66.162.88.83 -iface ppp0
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#!/bin/bash
echo '
nat on lo0 from en4:network to any port 80 -> (lo0)
nat on lo0 from ppp0:network to any port 80 -> (lo0)
rdr pass on lo0 inet proto tcp from any to any port 80 -> 127.0.0.1 port 8080
rdr pass on en4 inet proto tcp from any to any port 80 -> 127.0.0.1 port 8080
rdr pass on ppp0 inet proto tcp from any to any port 80 -> 127.0.0.1 port 8080
block in all
pass in on lo0
pass out keep state
' | sudo pfctl -F all -f -

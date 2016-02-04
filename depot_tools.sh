#!/bin/bash

if [ ! -d "$HOME/mybins/depot_tools" ]; then
    git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git "$HOME/mybins/depot_tools"
fi

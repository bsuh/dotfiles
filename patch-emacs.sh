#!/bin/sh

brew tap railwaycat/emacsmacport
git -C /usr/local/Library/Taps/railwaycat/homebrew-emacsmacport/Formula/ reset --hard HEAD
git -C /usr/local/Library/Taps/railwaycat/homebrew-emacsmacport/Formula/ apply ~/.cider/emacs-mac-formula.diff

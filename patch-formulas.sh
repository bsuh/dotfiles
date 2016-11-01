#!/bin/sh

git -C /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/ reset --hard HEAD
git -C /usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/ apply ~/.cider/formulas.diff

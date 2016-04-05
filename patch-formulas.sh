#!/bin/sh

git -C /usr/local/Library/Taps/homebrew/homebrew-core/ reset --hard HEAD
git -C /usr/local/Library/Taps/homebrew/homebrew-core/ apply ~/.cider/formulas.diff

#!/bin/sh

git -C /usr/local/ reset --hard HEAD
git -C /usr/local/ apply ~/.cider/formulas.diff

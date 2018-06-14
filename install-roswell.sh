#!/usr/bin/env sh
#
# Dependencies:
#  - libcurl (both v3 and v4 should work) : used for downloading the lisp implimentation binaries etc.
#  - automake (required when building from the source)
#  - developmental headers of libcurl (required when building from the source)

set -e
set -x

[ -d ~/cache ] && mkdir -p ~/cache
git clone -b release https://github.com/snmsts/roswell.git ~/cache/roswell
cd ~/cache/roswell
sh bootstrap
./configure
make
sudo make install

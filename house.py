#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "python36.withPackages(ps: [ps.python3-phue-1.1])"

from phue import Bridge

print("ok")

#! /usr/bin/env nix-shell
#! nix-shell -i python -p "python3.withPackages(p: [p.matplotlib])"

import re
import sys
import math
import argparse
import fileinput
import matplotlib.pyplot as plt

# Complex regex that grabs two match groups: category and %, but it ignores rows
# with a headline that starts with "\_", thereby selecting only the top-level
# agenda items.
clocktable_row = re.compile(
    r"^\|\s*\|\s(\w*(?!.*\_)).*\|.*\s+(\d+\.\d)\s\|$",
    re.MULTILINE,
)

if __name__ == '__main__':
    cli = argparse.ArgumentParser(description='Generate a pie chart from your org agenda clocktable')
    cli.add_argument('infile', type=argparse.FileType('r'), nargs='?', default='-')
    cli.add_argument('outfile', type=argparse.FileType('w'), nargs='?', default=None)
    args = cli.parse_args()

    content = args.infile.read()
    matches = re.finditer(clocktable_row, content)

    slices = {}
    for m in matches:
        if m.group(1) == "ALL": continue
        k = m.group(1)
        v = float(m.group(2))
        if k not in slices:
            slices[k] = v
        else:
            slices[k] += v

    # Round up b/c of floating point arithmetic
    total = math.ceil(sum(slices.values()))
    if total != 100:
        print("Warning: Sum of the slices is {} but it should be 100!".format(total))

    fig, ax = plt.subplots()
    wedges, texts, autotexts = ax.pie(
        slices.values(),
        labels = slices.keys(),
        autopct='%1.1f%%',
        textprops=dict(color="w"))

    ax.legend(wedges, slices.keys(), title="Categories", loc="center left",
              bbox_to_anchor=(0, 0, 0.5, 1))

    ax.axis('equal') # Equal aspect ratio ensures pie is drawn as a circle
    plt.setp(autotexts, size=8, weight="bold")
    ax.set_title("How I spent my time")

    if args.outfile is None:
        plt.show()
    else:
        plt.savefig(args.outfile.name)

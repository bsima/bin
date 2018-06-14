#!/usr/bin/env python3

import os
import re

year_re = r"(^\d{4})\d{2}\d{2}$"
month_re = r"^\d{4}(\d{2})\d{2}$"
day_re = r"^\d{4}\d{2}(\d{2}$)"

files = os.listdir(os.path.expanduser("~/me/journal"))
for f in files:
    print("working: ", f)
    if '.' in f:
        continue
    year = re.findall(year_re, f)[0]
    month = re.findall(month_re, f)[0]
    day = re.findall(day_re, f)[0]
    os.rename(
        "/home/ben/me/journal/{0}".format(f),
        "/home/ben/me/journal/{0}.{1}.{2}".format(year, month, day)
    )

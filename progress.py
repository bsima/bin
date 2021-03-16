#! /usr/bin/env python3

from datetime import datetime, timedelta
from calendar import *


class Progress():
    """Displays the time left in the day/workweek/month/year as a
    percentage. Inspired by https://twitter.com/year_progress

    Days are workdays (8am-5pm).

    """

    def __init__(self):
        self.now = datetime.now()
        weekdays_left = timedelta(7 - self.now.isoweekday())
        cal = Calendar()
        month_max = max(list(cal.itermonthdays(self.now.year, self.now.month)))
        year_max = 366 if isleap(self.now.year) else 365
        self.dura = {
            "day": timedelta(hours=9), # a workday is 9 hours long
            "week": timedelta(days=7),
            "month": timedelta(days=month_max),
            "year": timedelta(days=year_max),
        }
        self.end = {
            "year": datetime(self.now.year + 1, 1, 1),
            # day ends at hour 17 (5pm), and since it's 9 hours long, it starts
            # at 8am
            "day": datetime(self.now.year, self.now.month, self.now.day, hour=17),
            "week": datetime(self.now.year, self.now.month, self.now.day)
            + weekdays_left,
            "month": datetime(self.now.year, self.now.month, month_max),
        }

    def calc(self, key):
        """This calculates the time left in the duration specified by `key`, which
        should be a string of either day, week, month, or year.

        """
        return 1 - ((self.end[key] - self.now) / self.dura[key])

    def __str__(self):
        """String representation of the progress calculation. I just print this to my
        emacs minibuffer.

        """
        return "d: {d:.0%} | w: {w:.0%} | m: {m:.0%} | y: {y:.0%}".format(
            d=self.calc("day"),
            w=self.calc("week"),
            m=self.calc("month"),
            y=self.calc("year"),
        )


if __name__ == "__main__":
    print(str(Progress()))

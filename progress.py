#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

from datetime import datetime, timedelta
from calendar import *

class Progress(object):
    """Displays the time left in the day/week/month/year as a percentage. Inspired
    by https://twitter.com/year_progress

    """

    def __init__(self):
        self.now = datetime.now()
        weekdays_left = timedelta(7 - self.now.isoweekday())
        cal = Calendar()
        month_max = max(list(cal.itermonthdays(self.now.year, self.now.month)))
        year_max = 366 if isleap(self.now.year) else 365
        self.dura = {
            'day': timedelta(hours=8),
            'week': timedelta(days=7),
            'month': timedelta(days=month_max),
            'year': timedelta(days=year_max),
        }
        self.end = {
            'year': datetime(self.now.year+1, 1, 1),
            'day': datetime(self.now.year, self.now.month, self.now.day, hour=16),
            'week': datetime(self.now.year, self.now.month, self.now.day) + weekdays_left,
            'month': datetime(self.now.year, self.now.month, month_max),
        }

    def calc(self, key):
        return 1 - ((self.end[key] - self.now) / self.dura[key])

    def __str__(self):
        return "d: {d:.1%} | w: {w:.1%} | m: {m:.1%} | y: {y:.1%}".format(
            d=self.calc('day'),
            w=self.calc('week'),
            m=self.calc('month'),
            y=self.calc('year'),
        )

if __name__ == '__main__':
    print(str(Progress()))

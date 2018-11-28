#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

from datetime import datetime, timedelta
from calendar import *

class Progress(object):
    dura = {
        'day': timedelta(days=1),
        'week': timedelta(days=7),
        'month': timedelta(days=30),
        'year': timedelta(days=365),
    }

    def __init__(self):
        self.now = datetime.now()
        weekdays_left = timedelta(7 - self.now.isoweekday())
        month_max = max(list(Calendar().itermonthdays(self.now.year, self.now.month)))
        self.end = {
            'year': datetime(self.now.year+1, 1, 1),
            'day': datetime(self.now.year, self.now.month, self.now.day, hour=17),
            'week': datetime(self.now.year, self.now.month, self.now.day) + weekdays_left,
            'month': datetime(self.now.year, self.now.month, month_max),
        }

    def _calc(self, key):
        return 1 - ((self.end[key] - self.now) / self.dura[key])

    def workday(self):
        return self._calc('day')

    def week(self):
        return self._calc('week')

    def month(self):
        return self._calc('month')

    def year(self):
        return self._calc('year')

    def __str__(self):
        return "d: {day:.1%} | w: {week:.1%} | m: {month:.1%} | y: {year:.1%}".format(
            day=self.workday(),
            week=self.week(),
            month=self.month(),
            year=self.year()
        )

if __name__ == '__main__':
    print(str(Progress()))

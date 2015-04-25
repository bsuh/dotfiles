#!bin/python

from bs4 import BeautifulSoup
import calendar
from colorama import init, Fore
import dateutil.parser
import os
import os.path
import requests
import sys
import time

if __name__ == '__main__':
    init()
    query = sys.argv[1]
    r = requests.get('http://www.nyaa.se/', params={
        'page': 'search',
        'cats': '1_37',
        #'filter': 2,
        'sort': 2,
        'term': query
        })
    soup = BeautifulSoup(r.text)

    for link in soup.find_all('a'):
        if 'page=download' in link.get('href').lower():
            #print link.get('href')
            r = requests.get(link.get('href'))
            assert r.ok
            disposition = r.headers['Content-Disposition']
            filename = disposition.split('filename=')[1].replace('"', '')
            date = dateutil.parser.parse(r.headers['Last-Modified'])

            if os.path.isfile(filename):
                print Fore.RED + 'Skipping: %s' % filename
                break

            print Fore.GREEN + filename
            with open(filename, 'w') as f:
                f.write(r.content)

            os.utime(filename, (time.time(), calendar.timegm(date.timetuple())))
            break
    else:
        print Fore.RED + 'Not found: %s' % query

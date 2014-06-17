#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import json, calendar, requests, time, urllib, os, random, zlib, thread

def thread_burn(thread_id):
    while True:
        random_length = (random.random() * 10000) + 1
        #compressed = "".join(["A" for i in range(0, int(random_length))])
        random_str = os.urandom(int(random_length))
        #compressed = zlib.compress(random_str)
        compressed = random_str

        random_key_str = os.urandom(10)
        quoted = urllib.quote(random_key_str)
        expiration = int(calendar.timegm(time.gmtime()) + (random.random() * 10))

        connection_str = "http://localhost:8080/oleg/{}".format(quoted)
        requests.post(connection_str,
            data=compressed,
            headers={
                "Content-Type": "text/html",
                "X-OlegDB-use-by": expiration})
        duff = requests.head(connection_str) # For code coverage
        if duff.status_code != 404:
            known_count = duff.headers['x-olegdb-rcrd-cnt']
        resp = requests.get(connection_str, stream=True)
        raw = resp.raw.read()

        if raw != compressed:
            print "Assertion error: {} does not equal {}".format(hash(raw), hash(compressed))
            print "Raw: {}".format(raw)
            print "Response status: {}".format(resp.status_code)
            print "Last known count: {}".format(known_count)

def main():
    known_count = 0
    for x in range(0,3):
        thread.start_new_thread(thread_burn, (x,))
    thread_burn(x+1)

if __name__ == '__main__':
    main()

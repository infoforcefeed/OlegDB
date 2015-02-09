#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import json, calendar, requests, time, urllib, os, random, zlib, thread

# Fills up the server real fast. Stops at 100,000 keys.
def fill_fast(thread_id):
    i = 0
    while True:
        random_str = os.urandom(16)
        connection_str = "".join(["http://localhost:38080/oleg/", str(thread_id), "_", str(i)])
        requests.post(connection_str, data=random_str)
        i = i + 1

        if i > 100000:
            break

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

        connection_str = "http://localhost:38080/oleg/{}".format(quoted)
        requests.post(connection_str,
            data=compressed,
            headers={
                "X-Olegdb-Use-By": expiration}
            )
        duff = requests.get(connection_str + "/_info") # For code coverage
        if duff.status_code not in [404, 500]:
            known_count = duff.headers['X-Olegdb-Rcrd-Cnt']
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
        #thread.start_new_thread(fill_fast, (x,))
    fill_fast(x+1)

if __name__ == '__main__':
    main()

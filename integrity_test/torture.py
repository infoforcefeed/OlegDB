#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import json, calendar, requests, time, urllib, os, random, zlib

def main():
    while True:
        random_length = random.random() * 10000
        compressed = "".join(["A" for i in range(0, int(random_length))])
        #random_str = os.urandom(int(random_length))
        #compressed = zlib.compress(random_str)

        random_key_str = os.urandom(10)
        quoted = urllib.quote(random_key_str)
        expiration = int(calendar.timegm(time.gmtime()) + (random.random() * 10))

        connection_str = "http://localhost:8080/{}".format(quoted)
        requests.post(connection_str,
            data=compressed,
            headers={
                "Content-Type": "text/html",
                "X-OlegDB-use-by": expiration})
        resp = requests.get(connection_str, stream=True)
        raw = resp.raw.read()
        try:
            assert(raw == compressed)
        except AssertionError as e:
            print "Assertion error: {} does not equal {}".format(hash(raw), hash(compressed))
            print "Raw: {}".format(raw)
            print "Response status: {}".format(resp.status_code)

if __name__ == '__main__':
    main()

#!/usr/bin/env python
import simplejson, sys, urllib, requests

def main(argv):
    if len(argv) < 2:
        print "You need to specify a v.0.1.1 AOL file."
        return 1
    opened = open(argv[1], "r")
    database = simplejson.load(opened)
    opened.close()

    for key,value in database.items():
        host_str = u"http://localhost:8080/oleg/{}".format(key)
        value_encoded = value.encode("utf-8")
        resp = requests.post(host_str, data=value_encoded)

        if resp is None or resp.status_code != 200:
            print "Something went wrong when posting " + key


if __name__ == '__main__':
    main(sys.argv)

#!/usr/bin/env python2
import requests
import sys

def main(args):
    opened = open(args[1], "rb")
    url = "http://localhost:38080/oleg/{}".format(args[2])

    requests.post(url, data=opened)
    print "Posted to {}.".format(url)

    opened.close()

if __name__ == "__main__":
    main(sys.argv)

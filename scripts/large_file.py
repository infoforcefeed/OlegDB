#!/usr/bin/env python2
import requests
import sys

def main(args):
    opened = open(args[1], "rb")
    url = "http://localhost:8080/oleg/{}".format(args[1])
    if len(args) != 3:
        requests.post(url, data=opened)
        print "Posted to {}.".format(url)
    else:
        requests.post(url, data=opened, headers={"Content-Type": args[2]})
        print "Posted to {} with content type {}".format(url, args[2])
    opened.close()

if __name__ == "__main__":
    main(sys.argv)

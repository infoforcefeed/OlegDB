#!/usr/bin/env python2
import requests
import sys

def main(args):
    opened = open(args[1], "rb")
    url = "http://localhost:8080/{}".format(args[1])
    requests.post(url, data=opened, headers={"Content-Type": args[2]})
    opened.close()
    print "Posted to {} with content type {}".format(url, args[2])

if __name__ == "__main__":
    main(sys.argv)

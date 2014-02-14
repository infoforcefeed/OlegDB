#!/usr/bin/env python2
import json, requests, urllib

def main():
    videos_j = open("./videos.json")
    videos = json.load(videos_j)
    for key, value in videos.iteritems():
        real_key = urllib.quote(key.encode('ascii', 'replace'))
        req = requests.post("http://localhost:8080/{}".format(real_key),
                data=json.dumps(value), headers={'content-type': 'application/json'})
        assert(req.status_code == 200)

        req = requests.get("http://localhost:8080/{}".format(real_key))
        try:
            assert(req.headers['content-type'] == 'application/json')
            assert(value == req.json())
        except AssertionError as e:
            print "Fucked up."
            print "Real key: {} Key: {} Value: {}".format(real_key, key, value)
            return -1

    print "Immediate get tests passed."
    for key, value in videos.iteritems():
        real_key = urllib.quote(key.encode('ascii', 'replace'))
        req = requests.get("http://localhost:8080/{}".format(real_key))
        try:
            assert(req.headers['content-type'] == 'application/json')
            assert(value == req.json())
        except AssertionError as e:
            print "Fucked up."
            print "Real key: {} Key: {} Value: {}".format(real_key, key, value)
            return -1
    print "Full integrity passed."

if __name__ == '__main__':
    main()

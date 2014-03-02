#!/usr/bin/env python2
import json, requests, urllib

def assert_key_is_good(resp, value, real_key, key):
    try:
        assert(resp.headers['content-type'] == 'application/json')
        assert(value == resp.json())
    except AssertionError as e:
        print "Fucked up."
        print "Real key: {} Key: {} Value: {}".format(real_key, key, value)
        return -1
    return 0

def enc_key(key):
    return urllib.quote(key.encode('ascii', 'replace'))

def main():
    videos_j = open("./videos.json")
    videos = json.load(videos_j)
    i = 0
    for key, value in videos.iteritems():
        real_key = enc_key(key)
        resp = requests.post("http://localhost:8080/{}".format(real_key),
                data=json.dumps(value), headers={'Content-Type': 'application/json'})
        assert(resp.status_code == 200)

        resp = requests.post("http://localhost:8080/{}".format(i),
                data=json.dumps(value), headers={'Content-Type': 'application/json'})
        assert(resp.status_code == 200)

        resp = requests.get("http://localhost:8080/{}".format(real_key))
        assert(resp.status_code == 200)
        assert(assert_key_is_good(resp, value, real_key, key) == 0)

        y = requests.get("http://localhost:8080/{}".format(i))
        assert(y.status_code == 200)
        assert(assert_key_is_good(y, value, i, key) == 0)
        i = i + 1

    i = 0
    print "Immediate get tests passed."
    for key, value in videos.iteritems():
        real_key = enc_key(key)

        y = requests.get("http://localhost:8080/{}".format(i))
        assert(y.status_code == 200)
        assert(assert_key_is_good(y, value, i, key)==0)
        i = i + 1

        #resp = requests.get("http://localhost:8080/{}".format(real_key))
        #assert(resp.status_code == 200)
        #assert(assert_key_is_good(resp, value, real_key, key) == 0)

    print "Full integrity passed."

if __name__ == '__main__':
    main()

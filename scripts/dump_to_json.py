#!/usr/bin/env python
import simplejson as json, sys, lz4, struct
from datetime import datetime

class FuckOffException(Exception):
    pass

def read_command(opened_file):
    # :3:JAR:21:10138merveill.es_root:9:text/html:8:00021800:4230:<random data here>
    # Read in first colon
    _ = opened_file.read(1)
    char = b""
    length = b""
    while char != None and char != b':':
        char = opened_file.read(1)
        if not char:
            raise FuckOffException()
        if char != b':':
            length = length + char

    cmd = opened_file.read(int(length))
    if not cmd:
        raise FuckOffException()

    return (length, cmd)

def main(argv):
    if len(argv) < 2:
        print "You need to specify a v.0.1.1 AOL file."
        return 1
    database = {}
    print "Opening {}".format(argv[1])
    v011_aol = open(argv[1], "rb")
    while(1):
        try:
            clen, cmd = read_command(v011_aol)
            klen, key = read_command(v011_aol)
            if cmd == 'JAR':
                ctlen, ctype = read_command(v011_aol)
                originallen, original_size = read_command(v011_aol)
                _ = v011_aol.read(1)
                char = b""
                length = b""
                while char != None and char != b':':
                    char = v011_aol.read(1)
                    if not char:
                        raise FuckOffException()
                    if char != b':':
                        length = length + char
                compressed = v011_aol.read(int(length))
                pre_compressed = struct.pack("I", int(original_size)) + compressed
                decompressed = lz4.decompress(pre_compressed)
                database[key] = decompressed.encode("base64")
            elif cmd == 'SPOIL':
                # :5:SPOIL:30:10391merveill.espage%3D15_root:20:2014-04-28T12:44:12Z
                datesize, date = read_command(v011_aol)
                expired = datetime.strptime(date, "%Y-%m-%dT%H:%M:%SZ")

                if expired <= datetime.now():
                    try:
                        del database[key]
                    except KeyError:
                        pass

            elif cmd == 'SCOOP':
                try:
                    del database[key]
                except KeyError:
                    pass

            v011_aol.read(1) # Newline.
        except FuckOffException:
            break

    v011_aol.close()

    output = argv[1] + ".json"
    opened_output = open(output, "w+")
    json.dump(database, opened_output, separators=(',', ':'))
    opened_output.close()

    return 0

if __name__ == '__main__':
    main(sys.argv)

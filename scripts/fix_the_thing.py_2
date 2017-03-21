#!/usr/bin/python

to_open = "oleg.aol"

def main():
    opened = open(to_open, "r")
    fixed = open("fixed.aol", "w")
    for line in opened:
        split_line = line.split(":")[1:]

        tups = zip(split_line[1::2], split_line[::2])

        chunks = []
        chunks.append("")
        for thing, _ in tups:
            stripped = thing.strip()
            chunks.append("{}:{}".format(len(stripped), stripped))

        joined = "{}{}".format(":".join(chunks), "\n")
        fixed.write(joined)


    fixed.close()
    opened.close()

if __name__ == '__main__':
    main()

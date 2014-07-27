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
            chunks.append("{}:{}".format(len(thing), thing))

        fixed.write(":".join(chunks))


    fixed.close()
    opened.close()

if __name__ == '__main__':
    main()

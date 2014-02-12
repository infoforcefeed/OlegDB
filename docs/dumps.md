Dumps
=====

Olegdb dumps are binary.

There is a header struct in [dump.h](./include/dump.h) that is written to the
beginning of the file.

10 bytes -> OLEGDBDUMP
3 bytes   -> Version number ascii
4 bytes   -> Record count
n bytes   -> data (keys and values)

Keys are always 16 bytes, so you read 16 bytes after reading the number of keys
to get the first bucket key. Following that is a size_t (8 bytes) value that is
the size of the data. We will call this data_size. Read data_size. This is the
key's data. Continue the process until Record count is exhausted.

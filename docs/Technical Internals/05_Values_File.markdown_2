The values file augments the [AOL file](#AOL_file) in persisting state to the
disk. The values file is basically all of your data, more or less aligned in
[four megabyte blocks](#VALUES_DEFAULT_SIZE).

Starting in `0.1.2`, this is how we store data on disk. Previously all values
were stored in the AOL file. Instead, the values file is `mmap()`'d into RAM
on database startup, allowing you to hold datasets bigger than memory.

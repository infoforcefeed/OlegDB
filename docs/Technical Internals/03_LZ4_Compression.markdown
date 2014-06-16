OlegDB uses the super-fast [LZ4 compression algorithm](https://code.google.com/p/lz4/)
for keeping values on disk in a smaller size while maintaining a low-impact on
insertion/deletion.

This is a toggleable feature. See [ol_feature_flags](#ol_feature_flags) for more
detail.

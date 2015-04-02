In addition to the hash table, OlegDB also keeps track of currently inserted
nodes via a [splay tree.](https://en.wikipedia.org/wiki/Splay_tree) Going over
the intricacies of splay trees is a little outside the scope of this
documentation, but we do use it for several things and for several reasons.

In their simplified form, splay trees are just a specialized form of a
[self-balancing binary search tree.](https://en.wikipedia.org/wiki/Binary_search_tree) 
This means that searching for any given key in the tree is an `O(log n)`
operation and can be done relatively quickly.

In addition to be a binary tree, a splay tree has the property of moving
recently inserted keys to the top of the tree. This is known as a [splaying
operation](https://en.wikipedia.org/wiki/Splay_tree#Splaying). While some splay
tree implementations splay on read, write and deletion, OlegDB only splays keys
to the top of the tree upon insertion and deletion. We figured that, since the
splay tree is at most a secondary structure in the Oleg ecosystem, we wanted
it's impact to be minimal.

With splay trees installed, we can now iterate through the tree in a timely and
efficient manor, and whats more, in a user-decided order.

Binary trees have [several modes of traversal](https://en.wikipedia.org/wiki/In-order_traversal)
that can be useful in a database context. Traversing the tree in-order gives the
user the ability to retrieve keys alphabetically, while traversing in a
pre-ordered fashion will show the user when the keys were inserted.

Besides key-traversal, splay trees are used for [prefix matching](#ol_prefix_match). 
Since binary trees are inherently sorted, we can iterate through one much faster
than we could a list.

Splay trees can be turned on/off by changing how you open a database. See
[ol_feature_flags](#ol_feature_flags) for a complete list of toggleable
parameters.

#pragma once
/* The MIT License (MIT)
* 
* Copyright (c) 2014 Quinlan Pfiffer, Kyle Terry
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
#include "defs.h"

/* Leaves of the splay tree */
typedef struct ol_splay_tree_node {
    struct  ol_splay_tree_node *left;
    struct  ol_splay_tree_node *right;
    struct  ol_splay_tree_node *parent;

    char    key[KEY_SIZE];
    size_t  klen;
} ol_splay_tree_node;

/* The actual splay tree object */
typedef struct ol_splay_tree {
    ol_splay_tree_node *root;
    int rcrd_cnt;
} ol_splay_tree;

/* Insert and splay the tree */
int ols_insert(ol_splay_tree *tree, const char *key, const size_t klen);
/* Delete and splay the tree */
int ols_delete(ol_splay_tree *tree, const char *key, const size_t klen);
/* Find an object in the tree. Returns NULL on failure to launch. */
ol_splay_tree_node *ols_find(ol_splay_tree *tree, const char *key, size_t klen);

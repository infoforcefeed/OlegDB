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

/* Leaves of the splay tree */
typedef struct ol_splay_tree_node {
    struct ol_splay_tree_node *left;
    struct ol_splay_tree_node *right;
    struct ol_splay_tree_node *parent;

    ol_bucket *bucket;
} ol_splay_tree_node;

/* The actual splay tree object */
typedef struct ol_splay_tree {
    ol_splay_tree_node *root;
} ol_splay_tree;

/* Insert and splay the tree */
int ols_insert(const ol_splay_tree *tree, ol_bucket *bucket);
/* Delete and splay the tree */
int ols_delete(const ol_splay_tree *tree, ol_bucket *bucket);
ol_bucket *ols_find(const ol_splay_tree *tree);

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

/* xXx DEFINE=VERSION xXx
* xXx DESCRIPTION=The current version of the OlegDB. xXx
*/
#define VERSION "0.1.0"

/* xXx DEFINE=KEY_SIZE xXx
* xXx DESCRIPTION=The hardcoded upperbound for key lengths. xXx
*/
#define KEY_SIZE 250

/* xXx DEFINE=HASH_MALLOC xXx
* xXx DESCRIPTION=The size, in bytes, to allocate when initially creating the database. ol_bucket pointers are stored here. xXx
*/
#define HASH_MALLOC 65536

/* xXx DEFINE=PATH_LENGTH xXx
* xXx DESCRIPTION=The maximum length of a database's path. xXx
*/
#define PATH_LENGTH 256

/* xXx DEFINE=DB_NAME_SIZE xXx
* xXx DESCRIPTION=Database maximum name length. xXx
*/
#define DB_NAME_SIZE 64

/* xXx DEFINE=DEVILS_SEED xXx
* xXx DESCRIPTION=The seed to feed into the murmur3 algorithm. xXx
*/
#define DEVILS_SEED 666


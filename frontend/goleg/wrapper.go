package goleg

/*
#cgo CFLAGS: -I../../include
#cgo LDFLAGS: -loleg
#include <stdlib.h>
#include "oleg.h"
#include "cursor.h"
*/
import "C"
import (
	"reflect"
	"time"
	"unsafe"
)

const F_APPENDONLY = C.OL_F_APPENDONLY
const F_LZ4 = C.OL_F_LZ4
const F_SPLAYTREE = C.OL_F_SPLAYTREE
const F_AOL_FFLUSH = C.OL_F_AOL_FFLUSH

func COpen(path, name string, features int) *C.ol_database {
	// Turn parameters into their C counterparts
	cpath := C.CString(path)
	defer C.free(unsafe.Pointer(cpath))

	cname := C.CString(name)
	defer C.free(unsafe.Pointer(cname))

	cfeats := C.int(features)

	// Pass them to ol_open
	return C.ol_open(cpath, cname, cfeats)
}

func CClose(database *C.ol_database) int {
	return int(C.ol_close(database))
}

func CUnjar(db *C.ol_database, key string, klen uintptr, dsize *uintptr) []byte {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)
	cdsize := (*C.size_t)(unsafe.Pointer(dsize))

	// Pass them to ol_unjar
	var ptr *C.uchar
	res := C.ol_unjar(db, ckey, cklen, &ptr, cdsize)
	if res == 1 {
		return nil
	}
	// Retrieve data in Go []bytes
	data := C.GoBytes(unsafe.Pointer(ptr), C.int(*dsize))

	// Free C pointer
	C.free(unsafe.Pointer(ptr))

	return data
}

func CJar(db *C.ol_database, key string, klen uintptr, value []byte, vsize uintptr) int {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)
	cvsize := (C.size_t)(vsize)

	cvalue := (*C.uchar)(unsafe.Pointer(&value[0]))

	// Pass them to ol_jar
	return int(C.ol_jar(db, ckey, cklen, cvalue, cvsize))
}

func CScoop(db *C.ol_database, key string, klen uintptr) int {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)

	// Pass them to ol_scoop
	return int(C.ol_scoop(db, ckey, cklen))
}

func CUptime(db *C.ol_database) int {
	return int(C.ol_uptime(db))
}

func CExpirationTime(db *C.ol_database, key string, klen uintptr) (time.Time, bool) {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)

	// Pass them to ol_expiration_time
	ctime := C.ol_expiration_time(db, ckey, cklen)

	// Does the expiration exist? If no, return false as second param
	if ctime == nil {
		return time.Now(), false
	}

	// Turn ctime into a Go datatype
	gotime := time.Date(int(ctime.tm_year)+1900,
		time.Month(int(ctime.tm_mon)+1),
		int(ctime.tm_mday),
		int(ctime.tm_hour),
		int(ctime.tm_min),
		int(ctime.tm_sec),
		0, time.Local)
	return gotime, true
}

func CSpoil(db *C.ol_database, key string, klen uintptr, expiration time.Time) int {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)

	exp := expiration

	var ctime C.struct_tm
	ctime.tm_year = C.int(exp.Year() - 1900)
	ctime.tm_mon = C.int(int(exp.Month()) - 1)
	ctime.tm_mday = C.int(exp.Day())
	ctime.tm_hour = C.int(exp.Hour())
	ctime.tm_min = C.int(exp.Minute())
	ctime.tm_sec = C.int(exp.Second())

	// Pass them to ol_spoil
	return int(C.ol_spoil(db, ckey, cklen, &ctime))

}

func CExists(db *C.ol_database, key string, klen uintptr) int {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)

	return int(C.ol_exists(db, ckey, cklen))
}

func CSquish(db *C.ol_database) int {
	return int(C.ol_squish(db))
}

func CCas(db *C.ol_database, key string, klen uintptr, value []byte, vsize uintptr, ovalue *[]byte, ovsize *uintptr) int {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)
	cvsize := (C.size_t)(vsize)
	covsize := (C.size_t)(*ovsize)

	cvalue := (*C.uchar)(unsafe.Pointer(&value[0]))
	covalue := (*C.uchar)(unsafe.Pointer(ovalue))

	// Pass them to ol_jar
	return int(C.ol_cas(db, ckey, cklen, cvalue, cvsize, covalue, covsize))
}

func CPrefixMatch(db *C.ol_database, prefix string, plen uintptr) (int, []string) {
	// Turn parameters into their C counterparts
	cprefix := C.CString(prefix)
	defer C.free(unsafe.Pointer(cprefix))

	cplen := (C.size_t)(plen)

	// Call native function
	var ptr C.ol_key_array
	length := int(C.ol_prefix_match(db, cprefix, cplen, &ptr))
	if length < 0 {
		return length, nil
	}

	// Set array structure
	hdr := reflect.SliceHeader{
		Data: uintptr(unsafe.Pointer(ptr)),
		Len:  length,
		Cap:  length,
	}
	strSlice := *(*[]*C.char)(unsafe.Pointer(&hdr))
	// Create GoString array
	out := make([]string, 0)
	for i := range strSlice {
		out = append(out, C.GoString(strSlice[i]))
		C.free(unsafe.Pointer(strSlice[i]))
	}
	// Free structure
	C.free(unsafe.Pointer(ptr))
	return length, out
}

func CDumpKeys(db *C.ol_database) (int, []string) {

	// Call native function
	var ptr C.ol_key_array
	length := int(C.ol_key_dump(db, &ptr))
	if length < 0 {
		return length, nil
	}

	// Set array structure
	hdr := reflect.SliceHeader{
		Data: uintptr(unsafe.Pointer(ptr)),
		Len:  length,
		Cap:  length,
	}
	strSlice := *(*[]*C.char)(unsafe.Pointer(&hdr))
	// Create GoString array
	out := make([]string, 0)
	for i := range strSlice {
		out = append(out, C.GoString(strSlice[i]))
		C.free(unsafe.Pointer(strSlice[i]))
	}
	// Free structure
	C.free(unsafe.Pointer(ptr))
	return length, out
}

func CGetBucket(db *C.ol_database, key string, klen uintptr, _key *string, _klen *uintptr) *C.ol_bucket {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	var c_key [C.KEY_SIZE]C.char

	cklen := (C.size_t)(klen)
	c_klen := (*C.size_t)(unsafe.Pointer(_klen))

	bucket := C.ol_get_bucket(db, ckey, cklen, &c_key, c_klen)

	*_key = C.GoStringN((*C.char)(unsafe.Pointer(&c_key)), C.int(*c_klen))

	return bucket
}

func CNodeFirst(db *C.ol_database) *C.ol_splay_tree_node {
	minimum := C.ols_subtree_minimum(db.tree.root)
	return minimum
}

func CNodeLast(db *C.ol_database) *C.ol_splay_tree_node {
	maximum := C.ols_subtree_maximum(db.tree.root)
	return maximum
}

func CNodeNext(db *C.ol_database, key string, klen uintptr) *C.ol_splay_tree_node {
	var _key string
	var _klen uintptr
	bucket := CGetBucket(db, key, klen, &_key, &_klen)

	if bucket == nil {
		return nil
	}

	node := bucket.node
	maximum := C.ols_subtree_maximum(db.tree.root)
	ret := int(C._olc_next(&node, maximum))
	if ret == 0 || node == bucket.node {
		return nil
	}

	return node
}

func CNodePrev(db *C.ol_database, key string, klen uintptr) *C.ol_splay_tree_node {
	var _key string
	var _klen uintptr
	bucket := CGetBucket(db, key, klen, &_key, &_klen)

	if bucket == nil {
		return nil
	}

	node := bucket.node
	minimum := C.ols_subtree_minimum(db.tree.root)
	ret := int(C._olc_prev(&node, minimum))
	if ret == 0 || node == bucket.node {
		return nil
	}

	return node
}

func CNodeGet(db *C.ol_database, node *C.ol_splay_tree_node) (bool, string, []byte) {
	// Get associated bucket
	bucket := (*C.ol_bucket)(node.ref_obj)

	if bucket == nil {
		return false, "", nil
	}

	// Get key and data
	key := C.GoStringN((*C.char)(unsafe.Pointer(&bucket.key)), C.int(bucket.klen))

	var dsize uintptr
	data := CUnjar(db, key, uintptr(len(key)), &dsize)

	return true, key, data
}

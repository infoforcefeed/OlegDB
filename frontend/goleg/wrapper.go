package goleg

/*
#cgo LDFLAGS: -loleg
#include <stdlib.h>
#include <oleg.h>
*/
import "C"
import "unsafe"
import "time"

const F_APPENDONLY = C.OL_F_APPENDONLY
const F_LZ4 = C.OL_F_LZ4
const F_SPLAYTREE = C.OL_F_SPLAYTREE

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

func CUnjar(db *C.ol_database, key string, klen uintptr, dsize uintptr) []byte {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)

	// Pass them to ol_unjar
	var ptr *C.uchar
	res := C.ol_unjar(db, ckey, cklen, &ptr)
	if res == 1 {
		return nil
	}
	// Retrieve data in Go []bytes
	data := C.GoBytes(unsafe.Pointer(ptr), C.int(dsize))

	// Free C pointer
	C.free(unsafe.Pointer(ptr))

	return data
}

func CUnjarDs(db *C.ol_database, key string, klen uintptr, dsize *uintptr) []byte {
	// Turn parameters into their C counterparts
	ckey := C.CString(key)
	defer C.free(unsafe.Pointer(ckey))

	cklen := (C.size_t)(klen)
	cdsize := (*C.size_t)(unsafe.Pointer(dsize))

	// Pass them to ol_unjar_ds
	var ptr *C.uchar
	res := C.ol_unjar_ds(db, ckey, cklen, &ptr, cdsize)
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

	// turn ctime into a Go datatype
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

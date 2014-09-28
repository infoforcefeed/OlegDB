package goleg

/*
#cgo CFLAGS: -I../../include
#cgo LDFLAGS: -L../../build/lib -loleg
#include "oleg.h"
*/
import "C"
import (
	"time"
)

type Database struct {
	db          *C.ol_database
	RecordCount *C.int
}

func Open(path, name string, features int) Database {
	var database Database
	database.db = COpen(path, name, features)
	database.RecordCount = &database.db.rcrd_cnt
	return database
}

func (d Database) Close() int {
	return CClose(d.db)
}

func (d Database) Unjar(key string) []byte {
	var dsize uintptr
	return CUnjar(d.db, key, uintptr(len(key)), &dsize)
}

func (d Database) Jar(key string, value []byte) int {
	return CJar(d.db, key, uintptr(len(key)), value, uintptr(len(value)))
}

func (d Database) Scoop(key string) int {
	return CScoop(d.db, key, uintptr(len(key)))
}

func (d Database) Uptime() int {
	return CUptime(d.db)
}

func (d Database) Expiration(key string) (time.Time, bool) {
	return CExpirationTime(d.db, key, uintptr(len(key)))
}

func (d Database) Spoil(key string, expiration time.Time) int {
	return CSpoil(d.db, key, uintptr(len(key)), expiration)
}

func (d Database) Exists(key string) bool {
	return CExists(d.db, key, uintptr(len(key))) == 0
}

func (d Database) Squish() bool {
	return CSquish(d.db) == 1
}

func (d Database) PrefixMatch(prefix string) (bool, []string) {
	len, out := CPrefixMatch(d.db, prefix, uintptr(len(prefix)))
	return len >= 0, out
}

func (d Database) First() (bool, string, []byte) {
	node := CNodeFirst(d.db)
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Last() (bool, string, []byte) {
	node := CNodeLast(d.db)
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Next(key string) (bool, string, []byte) {
	node := CNodeNext(d.db, key, uintptr(len(key)))
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Prev(key string) (bool, string, []byte) {
	node := CNodePrev(d.db, key, uintptr(len(key)))
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

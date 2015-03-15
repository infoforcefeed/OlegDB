package goleg

/*
#include "oleg.h"
*/
import "C"
import (
	"errors"
	"sync"
	"time"
)

type Database struct {
	db          *C.ol_database
	RecordCount *C.int
	mutex       *sync.Mutex
}

func Open(path, name string, features int) (Database, error) {
	var database Database
	database.db = COpen(path, name, features)
	if database.db == nil {
		return Database{}, errors.New("Can't open database (NULL returned)")
	}
	database.RecordCount = &database.db.rcrd_cnt
	database.mutex = &sync.Mutex{}
	return database, nil
}

func (d Database) Close() int {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CClose(d.db)
}

func (d Database) Unjar(key string) []byte {
	d.mutex.Lock()
	var dsize uintptr
	var ret = CUnjar(d.db, key, uintptr(len(key)), &dsize)
	d.mutex.Unlock()
	return ret
}

func (d Database) Jar(key string, value []byte) int {
	d.mutex.Lock()
	var ret = CJar(d.db, key, uintptr(len(key)), value, uintptr(len(value)))
	d.mutex.Unlock()
	return ret
}

func (d Database) Scoop(key string) int {
	d.mutex.Lock()
	var ret = CScoop(d.db, key, uintptr(len(key)))
	d.mutex.Unlock()
	return ret
}

func (d Database) Uptime() int {
	d.mutex.Lock()
	var ret = CUptime(d.db)
	d.mutex.Unlock()
	return ret
}

func (d Database) Expiration(key string) (time.Time, bool) {
	d.mutex.Lock()
	var time, exists = CSniff(d.db, key, uintptr(len(key)))
	d.mutex.Unlock()
	return time, exists
}

func (d Database) Spoil(key string, expiration time.Time) int {
	d.mutex.Lock()
	var ret = CSpoil(d.db, key, uintptr(len(key)), expiration)
	d.mutex.Unlock()
	return ret
}

func (d Database) Exists(key string) bool {
	d.mutex.Lock()
	var ret = CExists(d.db, key, uintptr(len(key))) == 0
	d.mutex.Unlock()
	return ret
}

func (d Database) Squish() bool {
	d.mutex.Lock()
	var ret = CSquish(d.db) == 1
	d.mutex.Unlock()
	return ret
}

func (d Database) PrefixMatch(prefix string) (bool, []string) {
	d.mutex.Lock()
	len, out := CPrefixMatch(d.db, prefix, uintptr(len(prefix)))
	d.mutex.Unlock()
	return len >= 0, out
}

func (d Database) DumpKeys() (bool, []string) {
	d.mutex.Lock()
	len, out := CDumpKeys(d.db)
	d.mutex.Unlock()
	return len >= 0, out
}

func (d Database) First() (bool, string, []byte) {
	d.mutex.Lock()
	node := CNodeFirst(d.db)
	if node == nil {
		d.mutex.Unlock()
		return false, "", nil
	}
	var exists, key, data = CNodeGet(d.db, node)
	d.mutex.Unlock()
	return exists, key, data
}

func (d Database) Last() (bool, string, []byte) {
	d.mutex.Lock()
	node := CNodeLast(d.db)
	if node == nil {
		d.mutex.Unlock()
		return false, "", nil
	}
	var exists, key, data = CNodeGet(d.db, node)
	d.mutex.Unlock()
	return exists, key, data
}

func (d Database) Next(key string) (bool, string, []byte) {
	d.mutex.Lock()
	node := CNodeNext(d.db, key, uintptr(len(key)))
	if node == nil {
		d.mutex.Unlock()
		return false, "", nil
	}
	var exists, next_key, data = CNodeGet(d.db, node)
	d.mutex.Unlock()
	return exists, next_key, data
}

func (d Database) Prev(key string) (bool, string, []byte) {
	d.mutex.Lock()
	node := CNodePrev(d.db, key, uintptr(len(key)))
	if node == nil {
		d.mutex.Unlock()
		return false, "", nil
	}
	var exists, prev_key, data = CNodeGet(d.db, node)
	d.mutex.Unlock()
	return exists, prev_key, data
}

//func (d Database) BulkUnjar(keys []string) ([]byte) {
//}

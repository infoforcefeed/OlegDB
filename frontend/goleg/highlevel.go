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
	defer d.mutex.Unlock()
	var dsize uintptr
	return CUnjar(d.db, key, uintptr(len(key)), &dsize)
}

func (d Database) Jar(key string, value []byte) int {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CJar(d.db, key, uintptr(len(key)), value, uintptr(len(value)))
}

func (d Database) Scoop(key string) int {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CScoop(d.db, key, uintptr(len(key)))
}

func (d Database) Uptime() int {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CUptime(d.db)
}

func (d Database) Expiration(key string) (time.Time, bool) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CExpirationTime(d.db, key, uintptr(len(key)))
}

func (d Database) Spoil(key string, expiration time.Time) int {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CSpoil(d.db, key, uintptr(len(key)), expiration)
}

func (d Database) Exists(key string) bool {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CExists(d.db, key, uintptr(len(key))) == 0
}

func (d Database) Squish() bool {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	return CSquish(d.db) == 1
}

func (d Database) PrefixMatch(prefix string) (bool, []string) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	len, out := CPrefixMatch(d.db, prefix, uintptr(len(prefix)))
	return len >= 0, out
}

func (d Database) DumpKeys() (bool, []string) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	len, out := CDumpKeys(d.db)
	return len >= 0, out
}

func (d Database) First() (bool, string, []byte) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	node := CNodeFirst(d.db)
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Last() (bool, string, []byte) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	node := CNodeLast(d.db)
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Next(key string) (bool, string, []byte) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	node := CNodeNext(d.db, key, uintptr(len(key)))
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

func (d Database) Prev(key string) (bool, string, []byte) {
	d.mutex.Lock()
	defer d.mutex.Unlock()
	node := CNodePrev(d.db, key, uintptr(len(key)))
	if node == nil {
		return false, "", nil
	}
	return CNodeGet(d.db, node)
}

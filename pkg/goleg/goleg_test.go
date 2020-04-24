package goleg

import (
	"bytes"
	"io/ioutil"
	"os"
	"strconv"
	"testing"
)

func openRandomDB(features int) (Database, string, error) {
	name, err := ioutil.TempDir("/tmp", "goleg")
	if err != nil {
		return Database{}, "", err
	}

	//F_APPENDONLY|F_AOL_FFLUSH|F_LZ4|F_SPLAYTREE
	database, err := Open(name, "test", features)
	if err != nil {
		return Database{}, "", err
	}

	return database, name, nil
}

func cleanTemp(dir string) {
	os.RemoveAll(dir)
}

func TestOpen(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping in short mode")
	}

	database, dir, err := openRandomDB(F_APPENDONLY)
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	database.Close()
	cleanTemp(dir)
}

const JARN = 10

func TestJar(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping in short mode")
	}

	database, dir, err := openRandomDB(F_LZ4 | F_SPLAYTREE)
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	for i := 0; i < JARN; i++ {
		if database.Jar("record"+strconv.Itoa(i), []byte("value"+strconv.Itoa(i))) != 0 {
			t.Fatalf("Can't jar value #%d", i)
		}
	}

	database.Close()
	cleanTemp(dir)
}

func TestUnjar(t *testing.T) {
	database, dir, err := openRandomDB(F_LZ4 | F_SPLAYTREE)
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	for i := 0; i < JARN; i++ {
		if database.Jar("record"+strconv.Itoa(i), []byte("value"+strconv.Itoa(i))) != 0 {
			t.Fatalf("Can't jar value #%d", i)
		}
	}

	for i := 0; i < JARN; i++ {
		val := database.Unjar("record" + strconv.Itoa(i))
		if !bytes.Equal(val, []byte("value"+strconv.Itoa(i))) {
			t.Errorf("Value #%d doesn't match", i)
		}
	}

	database.Close()
	cleanTemp(dir)
}

func TestFullKeyDump(t *testing.T) {
	database, _, err := openRandomDB(F_LZ4 | F_SPLAYTREE)
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	for i := 0; i < JARN; i++ {
		if database.Jar("record"+strconv.Itoa(i), []byte("value"+strconv.Itoa(i))) != 0 {
			t.Fatalf("Can't jar value #%d", i)
		}
	}

	gotKeys, keys := database.DumpKeys()

	if !gotKeys {
		t.Fatal("Didn't get keys and should have")
	}

	var j int
	for i := 0; i <= JARN; i++ {
		for _, key := range keys {
			if key == "record"+strconv.Itoa(i) {
				j++
			}
		}
	}
	if j != JARN {
		t.Fatal("One or more keys did not dump")
	}
}

func TestBulkUnjarOnlyReturnsKeysWeGiveIt(t *testing.T) {
	database, _, err := openRandomDB(F_LZ4 | F_SPLAYTREE)
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	keys := []string{"key0", "key1", "key2", "key3"}

	for i, key := range keys {
		if database.Jar(key, []byte("value"+strconv.Itoa(i))) != 0 {
			t.Fatalf("Can't jar value #%d", i)
		}
	}

	subset := keys[1:] //sans key0

	values := database.BulkUnjar(subset)

	if l := len(values); l != 3 {
		t.Fatalf("Expected a length of 3, got %d", l)
	}

	for i, value := range values {
		if subset[i][3] != string(value)[5] {
			t.Fatalf("Expected %b, got %b", subset[i][3], string(value)[5])
		}
	}
}

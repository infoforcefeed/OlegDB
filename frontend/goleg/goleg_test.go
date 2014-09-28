package goleg

import (
	"io/ioutil"
	"os"
	"testing"
)

func openRandomDB() (Database, string, error) {
	name, err := ioutil.TempDir("/tmp", "goleg")
	if err != nil {
		return Database{}, "", err
	}

	database, err := Open(name, "test", F_APPENDONLY|F_AOL_FFLUSH|F_LZ4|F_SPLAYTREE)
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

	database, dir, err := openRandomDB()
	if err != nil {
		t.Fatalf("Can't open database: %s", err.Error())
	}

	database.Close()
	cleanTemp(dir)
}

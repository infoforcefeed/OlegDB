package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"

	"github.com/infoforcefeed/olegdb/pkg/goleg"
)

type Config struct {
	// Base settings (required)
	Listen  string `json:"listen"`
	DataDir string `json: "datadir"`
	// HTTPS settings
	UseHTTPS         bool   `json:"usehttps"`
	CertFile         string `json:"certfile"`
	PkeyFile         string `json:"pkeyfile"`
	SplayTreeEnabled bool   `json:"splaytreeenabled"`
	LZ4Enabled       bool   `json:"lz4enabled"`
	AOLEnabled       bool   `json:"aolenabled"`
}

// Used to request databases from the global list.
type DBOpenRequest struct {
	DBName        string
	SenderChannel chan DBOpenResponse
}

type DBOpenResponse struct {
	Database goleg.Database
	DBError  error
}

var config Config
var dbOpenChannel chan DBOpenRequest
var databases map[string]goleg.Database

const (
	Usage = `
Usage: %s -config <config path> [options]
	Config path:
		Path to a configuration file. This is required.
Options:
	-v
		Version
	-h
		This help.
	-bind
		Override Listen directive in configuration.
	-dir
		Override db storage location in configuration.
	-enable-lz4
		Enables LZ4 compression.
	-enable-splay-tree
		Enables the splay tree.
	-enable-aol
		Enables the append-only log.
`
)

func main() {
	// Parse command line flags (if there are)
	directory := flag.String("dir", "", "Directory where to store dumps and data")
	configfile := flag.String("config", "", "Config file to read settings from")
	bindaddr := flag.String("bind", "", "Address and port to bind, host:port")
	splaytreeenabled := flag.Bool("enable-splay-tree", false, "Enables the splay tree")
	lz4enabled := flag.Bool("enable-lz4", false, "Enables LZ4 compression")
	aolenabled := flag.Bool("enable-aol", false, "Enables the append-only log")

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt)
	go func() {
		<-signals
		unload()
		os.Exit(0)
	}()

	flag.Parse()

	if *configfile == "" {
		fmt.Println("Config file is required")
		fmt.Printf(Usage, os.Args[0])
		os.Exit(1)
	}

	// Parse config file
	rawconf, err := ioutil.ReadFile(*configfile)
	if err != nil {
		fmt.Println("Could not read configuration. Please see the documentation.")
		fmt.Printf(Usage, os.Args[0])
		os.Exit(1)
	}
	err = json.Unmarshal(rawconf, &config)
	if err != nil {
		panic(err.Error())
	}

	databases = make(map[string]goleg.Database)

	// Override config option with command line ones
	if *directory != "" {
		config.DataDir = *directory
	}

	if *bindaddr != "" {
		config.Listen = *bindaddr
	}

	if *splaytreeenabled {
		config.SplayTreeEnabled = *splaytreeenabled
	}

	if *lz4enabled {
		config.LZ4Enabled = *lz4enabled
	}

	if *aolenabled {
		config.AOLEnabled = *aolenabled
	}

	http.HandleFunc("/", handler)
	defer unload()
	var httperr error
	log.Println("Starting server...")
	if config.LZ4Enabled {
		log.Println("LZ4 compression is enabled")
	}
	if config.AOLEnabled {
		log.Println("Append-only log is enabled")
	}
	if config.SplayTreeEnabled {
		log.Println("Splay-tree is enabled")
	}

	dbOpenChannel = DBRequester()

	if config.UseHTTPS {
		log.Println("Listening on https://" + config.Listen)
		httperr = http.ListenAndServeTLS(config.Listen, config.CertFile, config.PkeyFile, nil)
	} else {
		log.Println("Listening on http://" + config.Listen)
		httperr = http.ListenAndServe(config.Listen, nil)
	}
	if httperr != nil {
		log.Fatal(httperr)
	}
}

func unload() {
	for v := range databases {
		log.Println("Closing " + v + "...")
		databases[v].Close()
		log.Println(v + " closed.")
	}
}

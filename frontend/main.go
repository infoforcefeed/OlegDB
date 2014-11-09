package main

import (
	"./goleg"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"log"
	"os"
)

type Config struct {
	// Base settings (required)
	Listen  string
	DataDir string
	// HTTPS settings
	UseHTTPS bool
	CertFile string
	PkeyFile string
}

var config Config
var databases map[string]goleg.Database

const (
Usage = `
Usage: %s -config <config path> [options]
	Config path:
		Path to a configuration file. This is required.
Options:
	-bind
		Override Listen directive in configuration.
	-dir
		Override db storage location in configuration.
	-v
		Version
	-h
		This help.
`
)


func main() {
	// Parse command line flags (if there are)
	directory := flag.String("dir", "", "Directory where to store dumps and data")
	configfile := flag.String("config", "olegdb.conf", "Config file to read settings from")
	bindaddr := flag.String("bind", "", "Address and port to bind, host:port")

	flag.Parse()

	if configfile == nil {
		text := fmt.Sprintf(Usage, os.Args[0])
		log.Print("Config file is required")
		log.Fatal(text)
	}

	// Parse config file
	rawconf, err := ioutil.ReadFile(*configfile)
	if err != nil {
		log.Fatal("Could not read configuration. Please see the documentation.")
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

	http.HandleFunc("/", handler)
	defer unload()
	fmt.Println("Listening on " + config.Listen)
	if config.UseHTTPS {
		http.ListenAndServeTLS(config.Listen, config.CertFile, config.PkeyFile, nil)
	} else {
		http.ListenAndServe(config.Listen, nil)
	}
}

func unload() {
	for v := range databases {
		databases[v].Close()
	}
}

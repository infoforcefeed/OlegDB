package main

import (
	"./goleg"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
)

type Config struct {
	// Base settings (required)
	Listen  string `json:"listen"`
	DataDir string `json: "datadir"`
	// HTTPS settings
	UseHTTPS bool   `json:"usehttps"`
	CertFile string `json:"certfile"`
	PkeyFile string `json:"pkeyfile"`
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
	configfile := flag.String("config", "", "Config file to read settings from")
	bindaddr := flag.String("bind", "", "Address and port to bind, host:port")

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

	http.HandleFunc("/", handler)
	defer unload()
	var httperr error
	log.Println("Starting server...")
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
		databases[v].Close()
	}
}

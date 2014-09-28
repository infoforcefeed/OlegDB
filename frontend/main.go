package main

import (
	"./goleg"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
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

func main() {
	// Parse command line flags (if there are)
	directory := flag.String("dir", "", "Directory where to store dumps and data")
	configfile := flag.String("config", "olegdb.conf", "Config file to read settings from")
	bindaddr := flag.String("bind", "", "Address and port to bind, host:port")

	flag.Parse()

	// Parse config file
	rawconf, err := ioutil.ReadFile(*configfile)
	if err != nil {
		panic(err.Error())
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

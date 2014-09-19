package main

import (
	"./goleg"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"
	"time"
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

func handler(w http.ResponseWriter, r *http.Request) {
	// Check for a valid url
	if r.URL.Path == "/" {
		http.Error(w, "http://"+r.Host+"/<database>/<key>", 400)
		return
	}
	params := strings.Split(r.URL.Path[1:], "/")
	if len(params) < 2 {
		http.Error(w, "next time do a proper request", 400)
		return
	}

	// Get parameters
	dbname := params[0]
	key := params[1]

	// Get the database if loaded, show error otherwise
	var database goleg.Database
	var ok bool
	if database, ok = databases[dbname]; !ok {
		databases[dbname] = goleg.Open(config.DataDir, dbname, goleg.F_APPENDONLY|goleg.F_LZ4|goleg.F_SPLAYTREE)
		//TODO Find a way to close unused databases
	}

	// Get request? Get the value
	if r.Method == "GET" {
		value := database.Unjar(key)
		// Check if the item existed
		if value == nil {
			http.Error(w, "Are you sure this is mayo?", 404)
			return
		}

		// Print value
		fmt.Fprintf(w, string(value))
		return
	}

	// Post request? Submit a value
	if r.Method == "POST" {
		value, err := ioutil.ReadAll(r.Body)
		if err != nil {
			http.Error(w, "Your post body is in klingon or something", 500)
			return
		}

		// Check if value already existed
		exists := database.Exists(key)

		res := database.Jar(key, value)
		if res == 0 {
			// Status 201 if created, 200 if updated
			if exists {
				w.WriteHeader(200)
			} else {
				w.WriteHeader(201)
			}
			fmt.Fprintf(w, "Got a full jar!")
		} else {
			http.Error(w, "Wait, the jar vanished!", 500)
			return
		}

		// Try to set expiration, if provided
		if eep, ok := r.Header["X-Olegdb-Use-By"]; ok {
			ep, err := strconv.Atoi(eep[0])
			if err != nil {
				http.Error(w, "The expiration date does not meet my hipster requirements", 400)
				return
			}
			date := time.Unix(int64(ep), 0)
			database.Spoil(key, date)
			fmt.Fprintf(w, "\r\nThe jar is spoiling!")
			return
		}

	}

	// Delete request? Delete a value
	if r.Method == "DELETE" {
		res := database.Scoop(key)
		if res == 0 {
			fmt.Fprintf(w, "Scooping done, that'll be $5.10")
			return
		} else {
			http.Error(w, "Got mayo all over my shirt!", 500)
			return
		}
	}

	// Head request? Get key info
	if r.Method == "HEAD" {
		// Does it even exists?
		if !database.Exists(key) {
			http.Error(w, "Are you sure this is mayo?", 404)
			return
		}

		// Get and set Expiration
		res, bl := database.Expiration(key)
		if bl == true {
			w.Header().Add("Expires", strconv.Itoa(int(res.UTC().Unix())))
		}

		// Add Record count
		w.Header().Add("X-Olegdb-Rcrd-Cnt", strconv.Itoa(int(*database.RecordCount)))

		fmt.Fprintf(w, "\r\n")
	}
}

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
	fmt.Println("Listening on " + config.Listen)
	if config.UseHTTPS {
		http.ListenAndServeTLS(config.Listen, config.CertFile, config.PkeyFile, nil)
	} else {
		http.ListenAndServe(config.Listen, nil)
	}
}

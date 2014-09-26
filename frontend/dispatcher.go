package main

import (
	"./goleg"
	"net/http"
	"strings"
)

type Operation struct {
	Database  *goleg.Database
	Key       string
	Operation string
}

type HTTPError struct {
	Code    int
	Message string
}

func handler(w http.ResponseWriter, r *http.Request) {
	dbname, key, opname, err := getRequestInfo(r)
	if err != nil {
		http.Error(w, err.Message, err.Code)
		return
	}

	// Get the database if loaded, show error otherwise
	var database goleg.Database
	var ok bool
	if database, ok = databases[dbname]; !ok {
		databases[dbname] = goleg.Open(config.DataDir, dbname, goleg.F_APPENDONLY|goleg.F_AOL_FFLUSH|goleg.F_LZ4|goleg.F_SPLAYTREE)
		database = databases[dbname]
	}

	operation := Operation{
		Database:  &database,
		Key:       key,
		Operation: opname,
	}

	switch operation.Operation {
	case OpGet:
		err = httpGet(w, operation)
	case OpSet:
		err = httpSet(w, operation, r)
	case OpInfo:
		err = httpInfo(w, operation)
	case OpDelete:
		err = httpDelete(w, operation)
	case OpPrefixMatch:
		err = httpMatch(w, operation)
	default:
		err = &HTTPError{Message: "I don't get what you're trying to do", Code: 400}
	}

	if err != nil {
		http.Error(w, err.Message, err.Code)
	}
}

func getRequestInfo(r *http.Request) (database, key, operation string, err *HTTPError) {
	params := strings.Split(r.URL.Path[1:], "/")
	if len(params) < 2 {
		return "", "", "", &HTTPError{Code: 400, Message: "Not enought arguments in URL"}
	}

	// Get parameters
	database = params[0]
	key = params[1]
	operation = OpGet

	// Get operation name, it can either be:
	// 1. An HTTP method that's not GET
	if strings.ToUpper(r.Method) != "GET" {
		operation = "/" + r.Method
	} else
	// 2. A third argument (Cursor iteration)
	if len(params) > 2 {
		operation = "." + params[2]
	} else
	// 3. A second argument starting with a _ (but not another _)
	if key[0] == '_' {
		// To get _key you do __key
		if key[1] == '_' {
			key = key[1:]
		} else {
			operation = key
			key = ""
		}
	}

	// Case insensitive
	operation = strings.ToLower(operation)

	return database, key, operation, nil
}

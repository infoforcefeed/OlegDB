package main

import (
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/infoforcefeed/olegdb/pkg/goleg"
)

type Operation struct {
	Database  *goleg.Database
	Keys      []string
	Operation string
}

type HTTPError struct {
	Code    int
	Message string
}

func DBRequester() chan DBOpenRequest {
	DBRequestChannel := make(chan DBOpenRequest)
	go func() {
		for {
			dbRequest := <-DBRequestChannel
			dbname := dbRequest.DBName

			var database goleg.Database
			var dberr error
			var ok bool
			if database, ok = databases[dbname]; !ok {
				var flags int

				if config.AOLEnabled {
					flags = flags | goleg.F_APPENDONLY
				}

				if config.LZ4Enabled {
					flags = flags | goleg.F_LZ4
				}

				if config.SplayTreeEnabled {
					flags = flags | goleg.F_SPLAYTREE
				}
				flags = flags | goleg.F_AOL_FFLUSH
				databases[dbname], dberr = goleg.Open(config.DataDir, dbname, flags)
				database = databases[dbname]
			}

			dbRequest.SenderChannel <- DBOpenResponse{
				Database: database,
				DBError:  dberr,
			}
		}
	}()
	return DBRequestChannel
}

func fetchDB(dbOpenChannel chan DBOpenRequest, dbname string) (goleg.Database, error) {
	c := make(chan DBOpenResponse)
	dbOpenChannel <- DBOpenRequest{DBName: dbname, SenderChannel: c}
	answer := <-c
	return answer.Database, answer.DBError
}

func handler(w http.ResponseWriter, r *http.Request) {
	dbname, keys, opname, err := getRequestInfo(r)
	if err != nil {
		http.Error(w, err.Message, err.Code)
		return
	}

	// Get the database if loaded, show error otherwise
	var database goleg.Database
	var dberr error

	database, dberr = fetchDB(dbOpenChannel, dbname)
	if dberr != nil {
		http.Error(w, "Cannot open database", 500)
		return
	}

	operation := Operation{
		Database:  &database,
		Keys:      keys,
		Operation: opname,
	}

	switch operation.Operation {
	case OpGet:
		err = httpGet(w, operation)
	case OpSet:
		err = httpSet(w, operation, r)
	case OpInfo:
		err = httpInfo(w, operation)
	case OpUptime:
		err = httpUptime(w, operation)
	case OpDelete:
		err = httpDelete(w, operation)
	case OpAllKeys:
		err = httpAll(w, operation)
	case OpPrefixMatch:
		err = httpMatch(w, operation)
	case OpCursorFirst:
		err = httpCurFirst(w, operation)
	case OpCursorLast:
		err = httpCurLast(w, operation)
	case OpCursorNext:
		err = httpCurNext(w, operation)
	case OpCursorPrev:
		err = httpCurPrev(w, operation)
	case OpBulkUnjar:
		err = httpBulkUnjar(w, operation)
	case OpSquish:
		err = httpSquish(w, operation)
	default:
		err = &HTTPError{Message: "I don't get what you're trying to do", Code: 400}
	}

	if err != nil {
		http.Error(w, err.Message, err.Code)
	}
}

func getRequestInfo(r *http.Request) (database string, keys []string, operation string, err *HTTPError) {
	params := strings.Split(r.URL.Path[1:], "/")
	if len(params) < 2 {
		return "", []string{""}, "", &HTTPError{Code: 400, Message: "The wind whispers through your empty forest."}
	}

	// Get parameters
	database = params[0]
	keys = []string{params[1]}
	operation = OpGet

	// Get operation name, it can either be:
	// 1. An HTTP method that's not GET
	if strings.ToUpper(r.Method) != "GET" {
		if keys[0] == "_bulk_unjar" {
			// grab keys from POST body
			body, err := ioutil.ReadAll(r.Body)
			if err != nil {
				// TODO(kt): This is the fucking worst. Please, lets fix this.
				return "", []string{""}, "", &HTTPError{Code: 400, Message: "The wind whispers through your empty forest. And I'm sorry."}
			}
			keys = strings.Split(string(body), "\n")
			operation = "_bulk_unjar"
		} else {
			operation = "/" + r.Method
		}
	} else
	// 2. A third argument (Cursor iteration)
	if len(params) > 2 {
		operation = "." + params[2]
	} else
	// 3. A second argument starting with a _ (but not another _)
	if len(keys[0]) > 0 && keys[0][0] == '_' {
		// To get _foobar you do __foobar
		// To explain more: keys can start with underscores, but driver will have to
		// escape them.
		if len(keys[0]) > 1 && keys[0][1] == '_' {
			keys = []string{keys[0][1:]}
		} else {
			operation = keys[0]
			keys = []string{""}
		}
	}

	// Case insensitive
	operation = strings.ToLower(operation)

	return database, keys, operation, nil
}

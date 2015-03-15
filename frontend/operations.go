package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"
	"time"
)

const (
	OpGet         = "/get"
	OpSet         = "/post"
	OpInfo        = "._info"
	OpDelete      = "/delete"
	OpAllKeys     = "_all"
	OpUptime      = "_uptime"
	OpCursorFirst = "_first"
	OpCursorLast  = "_last"
	OpCursorNext  = "._next"
	OpCursorPrev  = "._prev"
	OpPrefixMatch = "._match"
	OpBulkUnjar   = "_bulk_unjar"
)

func httpGet(w http.ResponseWriter, op Operation) *HTTPError {
	value := op.Database.Unjar(op.Key)
	// Check if the item existed
	if value == nil {
		return &HTTPError{Code: 404, Message: "These aren't your ghosts."}
	}

	// Send value
	w.Header().Add("Content-Length", strconv.Itoa(len(value)))
	w.Write(value)
	return nil
}

func httpSet(w http.ResponseWriter, op Operation, r *http.Request) *HTTPError {
	value, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return &HTTPError{Code: 500, Message: "Your post body is messed up!"}
	}

	// Check if value already existed
	exists := op.Database.Exists(op.Key)

	res := op.Database.Jar(op.Key, value)
	if res == 0 {
		// Status 201 if created, 200 if updated
		if exists {
			w.WriteHeader(200)
		} else {
			w.WriteHeader(201)
		}
		fmt.Fprintf(w, "無駄")
	} else {
		return &HTTPError{Code: 500, Message: "Something went horribly wrong..."}
	}

	// Try to set expiration, if provided
	if eep, ok := r.Header["X-Olegdb-Use-By"]; ok {
		ep, err := strconv.Atoi(eep[0])
		if err != nil {
			return &HTTPError{Code: 500, Message: "The expiration format is wrong!"}
		}
		date := time.Unix(int64(ep), 0)
		op.Database.Spoil(op.Key, date)
		// fmt.Fprintf(w, "\r\nThe jar is spoiling!")
	}

	return nil
}

func httpInfo(w http.ResponseWriter, op Operation) *HTTPError {
	// Does it even exists?
	if !op.Database.Exists(op.Key) {
		return &HTTPError{Code: 404, Message: "Key not found in database"}
	}

	// Get and set Expiration
	res, doesExpire := op.Database.Expiration(op.Key)
	if doesExpire {
		w.Header().Add("Expires", strconv.Itoa(int(res.UTC().Unix())))
	}

	// Add Record count
	w.Header().Add("X-Olegdb-Rcrd-Cnt", strconv.Itoa(int(*op.Database.RecordCount)))

	// Send empty body
	fmt.Fprintf(w, "\r\n")
	return nil
}

func httpUptime(w http.ResponseWriter, op Operation) *HTTPError {
	res := op.Database.Uptime()

	fmt.Fprintf(w, "%d", res)
	return nil
}

func httpDelete(w http.ResponseWriter, op Operation) *HTTPError {
	res := op.Database.Scoop(op.Key)
	if res != 0 {
		return &HTTPError{Code: 500, Message: "Something went horribly wrong..."}
	}

	fmt.Fprintf(w, "Key deleted successfully!")
	return nil
}

func httpMatch(w http.ResponseWriter, op Operation) *HTTPError {
	has, res := op.Database.PrefixMatch(op.Key)
	if !has {
		return &HTTPError{Code: 404, Message: "No matches found"}
	}
	w.Header().Add("X-Olegdb-Num-Matches", strconv.Itoa(len(res)))
	content := strings.Join(res, "\n")
	w.Header().Add("Content-Length", strconv.Itoa(len(content)))
	fmt.Fprintf(w, content)
	return nil
}

func httpAll(w http.ResponseWriter, op Operation) *HTTPError {
	has, res := op.Database.DumpKeys()
	if !has {
		return &HTTPError{Code: 404, Message: "Could not dump keys. (Sharks om the beach?)"}
	}
	w.Header().Add("X-Olegdb-Num-Matches", strconv.Itoa(len(res)))
	content := strings.Join(res, "\n")
	w.Header().Add("Content-Length", strconv.Itoa(len(content)))
	fmt.Fprintf(w, content)
	return nil
}

func httpCurFirst(w http.ResponseWriter, op Operation) *HTTPError {
	has, key, data := op.Database.First()
	if !has {
		return &HTTPError{Code: 404, Message: "No records found"}
	}
	w.Header().Add("X-Olegdb-Key", key)
	w.Write(data)
	return nil
}

func httpCurLast(w http.ResponseWriter, op Operation) *HTTPError {
	has, key, data := op.Database.Last()
	if !has {
		return &HTTPError{Code: 404, Message: "No records found"}
	}
	w.Header().Add("X-Olegdb-Key", key)
	w.Write(data)
	return nil
}

func httpCurNext(w http.ResponseWriter, op Operation) *HTTPError {
	has, key, data := op.Database.Next(op.Key)
	if !has {
		return &HTTPError{Code: 404, Message: "No records found"}
	}
	w.Header().Add("X-Olegdb-Key", key)
	w.Write(data)
	return nil
}

func httpCurPrev(w http.ResponseWriter, op Operation) *HTTPError {
	has, key, data := op.Database.Prev(op.Key)
	if !has {
		return &HTTPError{Code: 404, Message: "No records found"}
	}
	w.Header().Add("X-Olegdb-Key", key)
	w.Write(data)
	return nil
}

func httpBulkUnjar(w http.ResponseWriter, op Operation) *HTTPError {
	matched_keys := op.Database.BulkUnjar(op.Key)
	// JSON OR SOME SHIT
	return nil
}

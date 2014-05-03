// based on http://golangtutorials.blogspot.com/2011/06/web-programming-with-go-first-web-hello.html
// run with `go run app.go`
// web app runs on `localhost:9999`

package main

import (
    "net/http" //package for http based web programs
    "fmt"
    "math"
    "strconv"
)

func home(w http.ResponseWriter, r *http.Request) { 
    //fmt.Println("Inside home()")
    fmt.Fprintf(w, "OK, no problema!")
}

func tetra(w http.ResponseWriter, r *http.Request) {
    //fmt.Println("Inside tetra()")
    xstr := r.URL.Path[len("/tetra/"):]
    // convert xstr to 64 bit float
    x, _ := strconv.ParseFloat(xstr, 64)
    fmt.Fprintf(w, "%g", math.Pow(x,x))
}

func main() {
    http.HandleFunc("/", home) 
    http.HandleFunc("/tetra/", tetra) 
    // Listen for connections at port 9999
    http.ListenAndServe("localhost:9052", nil)
}

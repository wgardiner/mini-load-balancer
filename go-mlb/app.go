package main

import (
    "net/http" //package for http based web programs
    "fmt"
    "math"
    "strconv"
)

func home(w http.ResponseWriter, r *http.Request) { 
    fmt.Fprintf(w, "OK, no problema!")
}

func tetra(w http.ResponseWriter, r *http.Request) {
    xstr := r.URL.Path[len("/tetra/"):]
    // convert xstr to 64 bit float
    x, _ := strconv.ParseFloat(xstr, 64)
    fmt.Fprintf(w, "%g", math.Pow(x,x))
}

func main() {
    http.HandleFunc("/", home) 
    http.HandleFunc("/tetra/", tetra) 
    // Listen for connections at port 9999
    http.ListenAndServe("localhost:9999", nil)
}

# Mini Load Balancer

## Overview
This project is an implementation of a simple web app in multiple languages and web frameworks:
- Ruby / Sinatra
- Python / Flask
- Node.js / Express
- GoLang / net-http
- Haskell / Scotty
The web app computes the tetration (`x**x`) for the requested number. So visiting `/tetra/3` results in 27. 

## Implementations

### Ruby Sinatra

```ruby
require 'rubygems'
require 'sinatra'

get '/' do
    "OK, no problema!"
end

get '/tetra/:xvar' do |x|
    "#{x.to_i ** x.to_i}"
end
```

### Python Flask

```python
from flask import Flask
app = Flask(__name__)

@app.route('/')
def home():
    return 'OK, no problema!'

@app.route('/tetra/<int:x>')
def tetra(x):
    return '%d' % (x**x)

if __name__ == '__main__':
    app.run()
```

### Node.js Express

```javascript
var express = require('express');
var app = express();

app.get('/', function(req, res){
    res.send('OK, no problema!');
});

app.get('/tetra/:x', function(req, res){
    var x = parseInt(req.params.x);
    var result = Math.pow(x,x);
    res.send('' + result);
});

  app.listen(3000);
```   

### Go net-http

```go
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
```

The Go implementation uses 64 bit floating point variables instead of integers...

### Haskell Scotty

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Format

main = scotty 4000 $ do
    get "/" $ do
        html $ mconcat ["OK, no problema!"]
    get "/tetra/:xvar" $ do
        xstr <- param "xvar"
        let x = read xstr :: Integer
        let f y = y^y
        Data.Text.Format.print "{}" $ Data.Text.Format.Only (f x)
        html $ "Tetration is output to console :/"
        return ()
```

### HAProxy Configuration

```apache
global
    maxconn 256
    nbproc 1
    log 127.0.0.1 local0

defaults
    mode http
    option httplog
    log global
    contimeout 5000
    clitimeout 50000
    srvtimeout 50000

frontend unsecured *:2000
    timeout client 5000
    mode http
    option httpclose
    option forwardfor #forward's clients IP to app
    default_backend www_backend

backend www_backend
    mode http
    option forwardfor #this sets X-Forwarded-For
    timeout server 30000
    timeout connect 4000
    server s0 localhost:2050 weight 1 maxconn 32 check
    server s1 localhost:2051 weight 1 maxconn 32 check
    server s2 localhost:2052 weight 1 maxconn 32 check
    server s3 localhost:2053 weight 1 maxconn 32 check
```

The Haskell implementation outputs the tetration result to the console, and just outputs "Tetration is output to console :/" to the html page.


## System Tests

All testing was performed on a laptop with an Intel Core 2 Duo P8800 CPU @ 2.66 GHz and 4 GB of RAM running Arch Linux 3.14.1.

### Baseline Tests
#### Computation Results: Total Execution Time in ms

| x   | Ruby | Python | Node.js | GoLang | Haskell |
| --- | ---- | ------ | ------- | ------ | ------- |
| 140 | 51   | 31     | 28      | 90 / 4 | 225 / 5 |
| 1e6 | 256  | 6050   | 26      | NA     | 229 / 7 |

Note :  GoLang was not used for x=1e6. The two values for x=140 are the times for `go run test.go` (compile and run code) and `./test` (run precompiled code)

#### Ruby
- Computation: 
    - `ruby -e '140**140'  0.04s user 0.01s system 91% cpu 0.051 total`
    - `ruby -e '1000000**1000000'  0.24s user 0.02s system 98% cpu 0.258 total`
- Performance:
    - Single Instance:
        - 20 users, 10 requests, tetra 10000: 
            - CPU: ~15%
            - Memory: ~1.2%
        - Idle
            - CPU: ~0
            - Memory: ~1.2%
    - 4 Instances:
        - 20 users, 10 requests, tetra 10000:
            - Ruby Instances:
                - CPU: ~3% each
                - Memory: ~1.2%
            - HAProxy 
                - CPU: ~0.8%
                - Memory: ~0.1
        - Idle
            - Ruby Instances:
                - CPU: ~0
                - Memory: ~0.1%

#### Python
- Computation: 
    - `python -c "140**140"  0.03s user 0.00s system 95% cpu 0.031 total`
    - `python -c "1000000**1000000"  6.05s user 0.03s system 99% cpu 6.097 total`

#### Node.js
- Computation: 
    - `node -e "Math.pow(140,140)"  0.01s user 0.01s system 82% cpu 0.028 total`
    - `node -e "Math.pow(1000000,1000000)"  0.01s user 0.01s system 78% cpu 0.026 total`

#### GoLang
- Computation: `go run test.go  0.05s user 0.03s system 92% cpu 0.090 total`

Testing code:
```go
package main
import("math")
func main() {
    result := math.Pow(140,140)
}
```

#### Haskell 
- Computation: 
    - Using runghc `runghc test.hs  0.21s user 0.02s system 90% cpu 0.248 total`
    - Using compiled code `./test  0.00s user 0.00s system 0% cpu 0.002 total `

Testing code:
```haskell
f :: Integer -> Integer
f x = x^x
main = do
    let y = (f 1000000)
    return ()
```

## Results
### Single Instance Results
Transactions per second for accessing: web root / tetra 140 /tetra 10000. Testing run using `siege` with 100 concurrent users, 100 requests each, and the benchmark flag set. 

| Request    | Ruby/Sinatra | Python/Flask | Node.js/Express | GoLang/net-http | Haskell/Scotty |
| ---------- | ------------ | ------------ | --------------- | --------------- | -------------- |
| `/`        | 890          | 932          | 2496            | 4651 / 9346     | 8929           |
| `/t/140`   | 835          | 911          | 2386            | 4464 / 9346     | 7812 *         |
| `/t/10000` | 203          | 18           | 2320            | NA              | 618 *          |


Notes:
- Haskell/Scotty is computing the tetration math, then sending hardcoded html. I had some trouble dealing with type conversion in Haskell and so I was able to print the tetration result to the console (which resulted in the process becoming IO bound, only getting around 50 transactions/s), but I couldn't send the tetration result as html. Thus I hardcoded the html responses to 140 and 10000, respectively, for benchmarking, though the tetration is still computed. The values listed are for compiled Haskell code.
- The values in the GoLang cells for `/` and `/t/140` represent the non-compiled (i.e. `go run app.go`) / compiled (i.e. `go build app.go`,`./app.go`) values, respectively. The tetra 10000 test was not run for Go because the implementation does not support such large numbers.
- It is also worth noting that I set `systemctl net.ipv4.tcp_tw_recycle=1` and `systemctl net.ipv4.tcp_tw_reuse=1` based on the article [Using Siege to Tune Apache on GNU/Linux](http://www.joedog.org/articles-tuning/) by Jeff Fulmer. 

### HAProxy Results (4 Instances)

Transactions per second for accessing: web root / tetra 140 /tetra 10000. Testing run using `siege` with 100 concurrent users, 100 requests each, and the benchmark flag set. 

| Request    | Ruby/Sinatra | Python/Flask | Node.js/Express | GoLang/net-http | Haskell/Scotty |
| ---------- | ------------ | ------------ | --------------- | --------------- | -------------- |
| `/`        | 1072         | 1012         | 2160            | 3278 / 5291     | 3058 / 5556    |
| `/t/140`   | 1019         | 968          | 2128            | 3164 / 5050     | NA             |
| `/t/10000` | 298          | 28           | 2137            | NA              | NA             |

Notes: 
- The two values ("x / y") in each of the GoLang and Haskell cells represent the non-compiled / compiled values, respectively.
- Testing for tetra 140 and tetra 10000 in Haskell was not performed.
- Testing for tetra 10000 in Go was not performed because the implementation does not support such large numbers.

## Remarks and Conclusions

- Python performed very poorly at large integer operations, i.e. tetra 10000.
- As might be expected, the compiled languages (i.e. Go and Haskell) performed superiorly to the interpretted languages.
- These implementations parallelized poorly on the machine used for testing. The Ruby and Python implementations showed marginal improvement (~10%) with 4 instance parallelization, but the Node.js marginally decreased performance (~10%), and the performance significantly decreased (~40%) in Go and Haskell. Perhaps having implemented the 4 servers in single Go and Haskell programs rather than 4 individual programs would have increased their parallel performance. 
- The Ruby implementation has the cleanest syntax. The Node.js program was easy to implement and performed relatively well, making it the "winner" in my eyes.

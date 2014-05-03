# Mini Load Balancer

## Overview
This project is an implementation of a simple web app in multiple languages and web frameworks:
- Ruby / Sinatra
- Python / Flask
- Node.js / Express
- GoLang / net-http
- Haskell / Scotty: this implementation doesn't actually work...

## System Tests
### Baseline Tests
#### Computation Results: Total Execution Time in ms

| x   | Ruby | Python | Node.js | GoLang | Haskell |
| --- | ---- | ------ | ------- | ------ | ------- |
| 140 | 51   | 31     | 28      | 90/4   | 
| 1e4 | 256  | 6050   | 26      | NA     |

__GoLang Note__:  GoLang was not used for x=1e4. The two values for x=140 are the times for `go run test.go` (compile and run code) and `./test` (run precompiled code)

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
- Performance:
    - 4 Instances, running 100 users, 100 requests each, tetra 1e4
    -


#### Node.js
- Computation: 
    - `node -e "Math.pow(140,140)"  0.01s user 0.01s system 82% cpu 0.028 total`
    - `node -e "Math.pow(1000000,1000000)"  0.01s user 0.01s system 78% cpu 0.026 total`

#### GoLang
- Computation: `go run test.go  0.05s user 0.03s system 92% cpu 0.090 total`
```go
package main
import("math")
func main() {
    result := math.Pow(140,140)
}
```

### Results
#### Single Instance Results
Transactions per second for accessing: web root / tetra 140 /tetra 10000 (except for Go, which used tetra 140). Testing run using siege with 100 concurrent users, 100 requests each, and the benchmark flag set. 
It is also worth noting that I set `systemctl net.ipv4.tcp_tw_recycle=1` and `systemctl net.ipv4.tcp_tw_reuse=1` based on the article [Using Siege to Tune Apache on GNU/Linux](http://www.joedog.org/articles-tuning/) by Jeff Fulmer. 

- Node.js / Express: 2494 / 2386 / 2320 
- Python / Flask: 932 / 911 / 18
- Ruby / Sinatra:  890 / 835 / 203
- Go / net-http: 4651 / 4464 / NA
- Haskell / Scotty: 4348 / NA / NA

| Request  | Ruby/Sinatra | Python/Flask | Node.js/Express | GoLang/net-http | Haskell/Scotty |
| -------- | ------------ | ------------ | --------------- | --------------- | -------------- |
| /        | 890          | 932          | 2496            | 4651            | 4348           |
| /t/140   | 835          | 911          | 2386            | 4464            | NA             |
| /t/10000 | 203          | 18           | 2320            | NA              | NA             |

__Moral of the Story__: don't use Python for large number crunching.

#### HAProxy Results (4 Instances)

- Ruby / Sinatra: 1072 / / 

| Request  | Ruby/Sinatra | Python/Flask | Node.js/Express | GoLang/net-http | Haskell/Scotty |
| -------- | ------------ | ------------ | --------------- | --------------- | -------------- |
| /        | 1072         | 1012         | xxxx            | xxxx            | xxxx           |
| /t/140   | 1019         | 968          | xxxx            | xxxx            | NA             |
| /t/10000 | 298          | 28           | xxxx            | NA              | NA             |

- Response to 20 users, 10 requests, tetra 10000
```
Transactions:                    200 hits
Availability:                 100.00 %
Elapsed time:                  11.67 secs
Data transferred:               7.63 MB
Response time:                  1.08 secs
Transaction rate:              17.14 trans/sec
Throughput:                     0.65 MB/sec
Concurrency:                   18.52
Successful transactions:         200
Failed transactions:               0
Longest transaction:            2.41
Shortest transaction:           0.09
```

- Response to 100 users, 10 requests, tetra 10000
```
Transactions:                   1000 hits
Availability:                 100.00 %
Elapsed time:                  59.98 secs
Data transferred:              38.15 MB
Response time:                  5.68 secs
Transaction rate:              16.67 trans/sec
Throughput:                     0.64 MB/sec
Concurrency:                   94.64
Successful transactions:        1000
Failed transactions:               0
Longest transaction:           10.17
Shortest transaction:           0.14
```

- Response to 100 users, 50 requests, tetra 10000
```
Transactions:                   5000 hits
Availability:                 100.00 %
Elapsed time:                 312.12 secs
Data transferred:             190.74 MB
Response time:                  6.14 secs
Transaction rate:              16.02 trans/sec
Throughput:                     0.61 MB/sec
Concurrency:                   98.33
Successful transactions:        5000
Failed transactions:               0
Longest transaction:           10.41
Shortest transaction:           0.10
```


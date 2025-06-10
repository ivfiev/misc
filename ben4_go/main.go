package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"time"
)

var FINISH bool = false

func sleep(secs int) {
  time.Sleep(time.Duration(secs) * time.Second)
  FINISH = true
}

func work_int(results chan int) {
	var primes [1229]int
	var count, k int
	for count = 0; !FINISH; count++ {
		for i := range len(primes) {
			primes[i] = 0
		}
		primes[0] = 2
		k = 1
		for n := 3; n < 10000; n += 2 {
			for i := range k {
				if n % primes[i] == 0 {
					break
				}
				if n < primes[i] * primes[i] {
					primes[k] = n
					k++
          break
				}
			}
		}
	}
	fmt.Printf("%d ", k)
  results <- count
}

func main() {
  if len(os.Args) < 2 {
    log.Fatalf("usage 'ben4_go #threads'")
  }
  threads, err := strconv.Atoi(os.Args[1])
  if err != nil {
    log.Fatalf("#threads [%s] must be a number\n", os.Args[0])
  }
  if threads < 0 || threads > 100 {
    log.Fatalf("bad #threads [%d]\n", threads)
  }
  sum := 0
  results := make(chan int)
	go sleep(10)
  for range threads {
    go work_int(results)
  }
  for range threads {
    res := <-results
    sum += res
  }
  fmt.Printf("\n%d\n", sum)
}

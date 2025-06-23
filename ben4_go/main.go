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

func work_int() int32 {
	var primes [1229]int32
	var count, k int32
	for count = 0; !FINISH; count++ {
		for i := range len(primes) {
			primes[i] = 0
		}
		primes[0] = 2
		k = 1
		for n := int32(3); n < 10000; n += 2 {
			for i := range k {
				if n%primes[i] == 0 {
					break
				}
				if n < primes[i]*primes[i] {
					primes[k] = n
					k++
					break
				}
			}
		}
	}
	fmt.Printf("%d ", k)
	return count
}

func work_float() int32 {
	var primes [1229]int32
	var count, k int32
	for count = 0; !FINISH; count++ {
		for i := range len(primes) {
			primes[i] = 0
		}
		primes[0] = 2
		k = 1
		for n := int32(3); n < 10000; n += 2 {
			for i := range k {
				if int32(float32(n)/float32(primes[i]))*primes[i] == n {
					break
				}
				if primes[i]*primes[i] > n {
					primes[k] = n
					k++
					break
				}
			}
		}
	}
	fmt.Printf("%d ", k)
	return count
}

func main() {
	if len(os.Args) < 3 {
		log.Fatalf("usage 'ben4_go i/f #threads'")
	}
	if os.Args[1] != "i" && os.Args[1] != "f" {
		log.Fatalf("incorrect numeric type %s\n", os.Args[1])
	}
	threads, err := strconv.Atoi(os.Args[2])
	if err != nil {
		log.Fatalf("#threads [%s] must be a number\n", os.Args[0])
	}
	if threads < 0 || threads > 100 {
		log.Fatalf("bad #threads [%d]\n", threads)
	}
	work := work_int
	if os.Args[1] == "f" {
		work = work_float
	}
	var sum int32 = 0
	results := make(chan int32)
	go sleep(10)
	for range threads {
		go func() {
			results <- work()
		}()
	}
	for range threads {
		sum += <-results
	}
	fmt.Printf("\n%d\n", sum)
}

package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"simd/archsimd"
	"strconv"
	"time"
)

func workInt(ctx context.Context) int32 {
	var primes [1229]int32
	var count, k int32
	for count = 0; ; count++ {
		select {
		case <-ctx.Done():
			fmt.Printf("%d ", k)
			return count
		default:
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
	}
}

func workFloat(ctx context.Context) int32 {
	var primes [1229]float32
	var recips [1229]float32
	var square [1229]float32
	var count, k int32
	for count = 0; ; count++ {
		select {
		case <-ctx.Done():
			fmt.Printf("%d ", k)
			return count
		default:
			for i := range len(primes) {
				primes[i] = 0
			}
			primes[0] = 2
			recips[0] = 0.5
			square[0] = 4
			k = 1
			for n := float32(3.0); n < 10000.0; n += 2.0 {
				for i := range k {
					if float32(int32(n*recips[i]+0.5))*primes[i] == n {
						break
					}
					if square[i] > n {
						primes[k] = n
						recips[k] = 1.0 / n
						square[k] = n * n
						k++
						break
					}
				}
			}
		}
	}
}

func workAvx(ctx context.Context) int32 {
	var primes [1229]float32
	var recips [1229]float32
	var square [1229]float32
	var count, k int
	for count = 0; ; count++ {
		select {
		case <-ctx.Done():
			fmt.Printf("%d ", k)
			return int32(count)
		default:
			for i := range len(primes) {
				primes[i] = 0
			}
			primes[0] = 2
			recips[0] = 0.5
			square[0] = 4
			k = 1
			for n := float32(3.0); n < 10000.0; n += 2.0 {
				ns := archsimd.BroadcastFloat32x16(n)
				for i := 0; i < k; {
					ps, di := archsimd.LoadFloat32x16Part(primes[i:])
					rs, _ := archsimd.LoadFloat32x16Part(recips[i:])
					ms := ns.Mul(rs).RoundScaled(0).Mul(ps).Equal(ns)
					bs := ms.ToBits()
					if bs > 0 {
						break
					}
					ss, _ := archsimd.LoadFloat32x16Part(square[i:])
					gt := ss.Greater(ns).ToBits()
					if gt > 0 {
						primes[k] = n
						recips[k] = 1.0 / n
						square[k] = n * n
						k++
						break
					}
					i += di
				}
			}
		}
	}
}

func main() {
	if len(os.Args) < 3 {
		log.Fatalf("usage 'ben4_go i/f/a #threads'")
	}
	if os.Args[1] != "i" && os.Args[1] != "f" && os.Args[1] != "a" {
		log.Fatalf("incorrect numeric type %s\n", os.Args[1])
	}
	threads, err := strconv.Atoi(os.Args[2])
	if err != nil {
		log.Fatalf("#threads [%s] must be a number\n", os.Args[0])
	}
	if threads < 0 || threads > 100 {
		log.Fatalf("bad #threads [%d]\n", threads)
	}
	work := workInt
	if os.Args[1] == "f" {
		work = workFloat
	}
	if os.Args[1] == "a" {
		work = workAvx
	}
	var sum int32 = 0
	results := make(chan int32)
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	for range threads {
		go func() {
			results <- work(ctx)
		}()
	}
	for range threads {
		sum += <-results
	}
	fmt.Printf("\n%d\n", sum)
}

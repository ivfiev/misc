package main

import (
	"fmt"
	"slices"
	"time"
)

func sim() {
	r := NewRandom(time.Now().UnixMilli())
	n := 10000
	ms := make([]*M, n)
	ws := make([]*W, n)
	for i := range n {
		ms[i] = RandomM(r)
		ws[i] = RandomW(r)
	}
	for range 365 {
		for _, m := range ms {
			m.Day(ws, r)
		}
	}
	slices.SortFunc(ms, func(m0, m1 *M) int {
		if len(m0.W) < len(m1.W) {
			return 1
		}
		return -1
	})
	fmt.Printf("\n%v\n", ms[0])
	fmt.Printf("%v\n", ms[n/10])
	fmt.Printf("%v\n", ms[n/2])
	fmt.Printf("%v\n", ms[n/10*8])
	fmt.Printf("%v\n", ms[n-1])
}

func main() {
	// sim()
}

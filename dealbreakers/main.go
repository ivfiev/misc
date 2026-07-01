package main

import (
	"fmt"
	"slices"
	"time"
)

func sim(w []float64) {
	r := NewRandom(time.Now().UnixMilli())
	n := 10000
	ms := make([]*M, n)
	ws := make([]*W, n)
	for i := range n {
		ms[i] = RandomM(r)
		ws[i] = RandomW(r, w)
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
	for i := range ms {
		slices.SortFunc(ms[i].W, func(w0, w1 *W) int {
			if w0.X[0] < w1.X[0] {
				return 1
			}
			return -1
		})
	}
	fmt.Printf("\n%v\n", ms[0])
	fmt.Printf("%v\n", ms[n/10])
	fmt.Printf("%v\n", ms[n/2])
	fmt.Printf("%v\n", ms[n/10*8])
	fmt.Printf("%v\n", ms[n-1])
}

func main() {
	r := NewRandom(42)
	w := train(r)
	println(fmtv(w))
	sim(w)
}

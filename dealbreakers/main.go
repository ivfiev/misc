package main

import (
	"fmt"
	"slices"
	"time"
)

func sim(wx, wy []float64) {
	r := NewRandom(time.Now().UnixMilli())
	n := 10000
	ms := make([]*M, n)
	ws := make([]*W, n)
	for i := range n {
		ms[i] = RandomM(r)
		ws[i] = RandomW(r, wx, wy)
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

func sim1(mx, my, wx, wy []float64) {
	r := NewRandom(time.Now().UnixMilli())
	n := 10000
	ws := make([]*W, n)
	for i := range n {
		ws[i] = RandomW(r, wx, wy)
	}
	m := NewM(mx, my)
	for range 365 {
		m.Day(ws, r)
	}
	fmt.Printf("%v\n", m)
}

func sim2(mx, my, wx, wy []float64) {
	ws := []*W{
		NewW([]float64{2, 2, 2}, []float64{0.5, 0.2, 0.3}, wx, []float64{1, 1}, wy),
		NewW([]float64{1, 1, 1}, []float64{0.2, 0.4, 0.4}, wx, []float64{-0.5, 1.5}, wy),
		NewW([]float64{3, -1, 0.5}, []float64{0.5, 0.3, 0.2}, wx, []float64{-2, 2}, wy),
		NewW([]float64{0.2, 0.5, -0.2}, []float64{0.3, 0.4, 0.3}, wx, []float64{0, -1}, wy),
		NewW([]float64{-1, 1, 0}, []float64{0.1, 0.4, 0.5}, wx, []float64{-1, -1}, wy),
	}
	m := NewM(mx, my)
	m.W = append(m.W, ws...)
	fmt.Printf("%v\n", m)
}

func main() {
	r := NewRandom(42)
	wx := trainWx(r)
	wy := trainWy(r)
	// println(fmtv(wx))
	// println(fmtv(wy))
	// sim(wx, wy)
	// sim1([]float64{2, 2, 2}, []float64{0.0, 2.0}, wx, wy)
	// sim1([]float64{0.5, 1.0, -2.0}, []float64{2.0, -2.0}, wx, wy)
	sim2([]float64{-2, -2, -2}, []float64{2, -2}, wx, wy)
	sim2([]float64{2, 2, 2}, []float64{-1, 1}, wx, wy)
	sim2([]float64{0.5, 1.0, -1.0}, []float64{1.5, -1.5}, wx, wy)
}

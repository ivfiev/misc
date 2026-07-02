package main

import (
	"fmt"
)

func spsa(w []float64, f func() float64, r *Random, eps, lr float64, steps int) {
	ones := make([]float64, len(w))
	grad := make([]float64, len(w))
	for range steps {
		for i := range ones {
			if r.U(0, 1) < 0.5 {
				ones[i] = -1
			} else {
				ones[i] = 1
			}
		}
		for i := range w {
			w[i] += ones[i] * eps
		}
		y1 := f()
		for i := range w {
			w[i] -= ones[i] * eps * 2
		}
		y0 := f()
		for i := range grad {
			grad[i] = (y1 - y0) / (ones[i] * eps * 2)
		}
		for i := range w {
			w[i] += ones[i] * eps
			w[i] -= lr * grad[i]
		}
	}
}

func train(r *Random) []float64 {
	const n = 32
	w := r.Ns(n, 0, 0.025)

	ws := make([]*W, 0)
	ms := make([]*M, 0)
	ys := make([]float64, 0)

	example := func(mx, wx, wp []float64, y float64) {
		ms = append(ms, NewM(mx))
		ws = append(ws, NewW(wx, wp, w))
		ys = append(ys, y)
	}

	example([]float64{0, 0, 0}, []float64{0, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.03)
	example([]float64{1, 0, 0}, []float64{0, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.07)
	example([]float64{1, 1, 1}, []float64{0, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.10)

	example([]float64{0, 0, 0}, []float64{1, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.01)
	example([]float64{0, 0, 0}, []float64{2, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.005)
	example([]float64{0, 0, 0}, []float64{3, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.001)

	example([]float64{0, 1, 0}, []float64{1, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.02)
	example([]float64{0, 2, 0}, []float64{2, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.04)
	example([]float64{0, 3, 0}, []float64{3, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.07)

	example([]float64{2, 0, 0}, []float64{0, 0, 0}, []float64{0.8, 0.1, 0.1}, 0.20)
	example([]float64{0, 2, 0}, []float64{0, 0, 0}, []float64{0.8, 0.1, 0.1}, 0.05)
	example([]float64{0, 0, 2}, []float64{0, 0, 0}, []float64{0.8, 0.1, 0.1}, 0.05)

	example([]float64{2, 0, 0}, []float64{0, 0, 0}, []float64{0.1, 0.8, 0.1}, 0.075)
	example([]float64{0, 2, 0}, []float64{0, 0, 0}, []float64{0.1, 0.8, 0.1}, 0.15)
	example([]float64{0, 0, 2}, []float64{0, 0, 0}, []float64{0.1, 0.8, 0.1}, 0.05)

	example([]float64{2, 0, 0}, []float64{0, 0, 0}, []float64{0.1, 0.1, 0.8}, 0.075)
	example([]float64{0, 2, 0}, []float64{0, 0, 0}, []float64{0.1, 0.1, 0.8}, 0.12)
	example([]float64{0, 0, 2}, []float64{0, 0, 0}, []float64{0.1, 0.1, 0.8}, 0.20)

	example([]float64{4, 4, 4}, []float64{0, 0, 0}, []float64{0.1, 0.1, 0.8}, 0.50)
	example([]float64{4, 4, 4}, []float64{0, 0, 0}, []float64{0.1, 0.8, 0.1}, 0.50)
	example([]float64{4, 4, 4}, []float64{0, 0, 0}, []float64{0.8, 0.1, 0.1}, 0.50)

	example([]float64{4, 4, 4}, []float64{4, 4, 4}, []float64{0.1, 0.1, 0.8}, 0.25)
	example([]float64{4, 4, 4}, []float64{4, 4, 4}, []float64{0.1, 0.8, 0.1}, 0.25)
	example([]float64{4, 4, 4}, []float64{4, 4, 4}, []float64{0.8, 0.1, 0.1}, 0.25)

	example([]float64{-1, 2, -2}, []float64{1, 1, 1}, []float64{0.2, 0.3, 0.5}, 0.01)
	example([]float64{-4, 2, 2}, []float64{1, 1, 1}, []float64{0.2, 0.3, 0.5}, 0.001)
	example([]float64{4, -2, -2}, []float64{2, 2, 2}, []float64{0.4, 0.3, 0.3}, 0.10)
	example([]float64{-1, 5, -1}, []float64{0, 0, 0}, []float64{0.05, 0.9, 0.05}, 0.50)

	example([]float64{-3, 2, 2}, []float64{0, 0, 0}, []float64{0.2, 0.4, 0.4}, 0.02)
	example([]float64{2, -3, 2}, []float64{0, 0, 0}, []float64{0.4, 0.2, 0.4}, 0.08)
	example([]float64{2, 2, -3}, []float64{0, 0, 0}, []float64{0.4, 0.4, 0.2}, 0.08)

	example([]float64{4, 0, 0}, []float64{0, 0, 0}, []float64{0.3, 0.4, 0.3}, 0.40)
	example([]float64{2, 2, 2}, []float64{0, 0, 0}, []float64{0.3, 0.4, 0.3}, 0.25)

	example([]float64{0, 1, 0}, []float64{0, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.06)
	example([]float64{0, 0, 1}, []float64{0, 0, 0}, []float64{0.333, 0.333, 0.333}, 0.06)
	example([]float64{0, 0, 1}, []float64{0, 0, 0}, []float64{0.1, 0.1, 0.8}, 0.09)
	example([]float64{1, -1, 1}, []float64{0, 0, 0}, []float64{0.4, 0.3, 0.3}, 0.08)

	example([]float64{-1, 0, 0}, []float64{3, 3, 3}, []float64{0.4, 0.3, 0.3}, 0.0005)
	example([]float64{0, -1, -1}, []float64{1, 1, 1}, []float64{0.4, 0.3, 0.3}, 0.005)
	example([]float64{-1, 2, 1}, []float64{2, -1, -1}, []float64{0.05, 0.9, 0.05}, 0.15)
	example([]float64{-1, 2, 1}, []float64{0, -2, 0}, []float64{0.01, 0.8, 0.19}, 0.18)
	example([]float64{-1, 1, 2}, []float64{2, -1, -1}, []float64{0.05, 0.05, 0.9}, 0.15)
	example([]float64{-1, 1, 2}, []float64{0, -2, 0}, []float64{0.01, 0.19, 0.8}, 0.18)

	example([]float64{-0.74, 0.71, -0.36}, []float64{-1.69, -1.55, -0.88}, []float64{0.2, 0.31, 0.49}, 0.08)
	example([]float64{-0.74, 0.71, -0.36}, []float64{-1.82, -1.28, -1.25}, []float64{0.19, 0.36, 0.45}, 0.10)
	example([]float64{-0.52, 1.34, 0.57}, []float64{-0.62, -3.07, 0.73}, []float64{0.2, 0.3, 0.5}, 0.12)
	example([]float64{-1.57, 0.88, -1.93}, []float64{-0.62, -3.07, 0.73}, []float64{0.2, 0.3, 0.5}, 0.02)
	example([]float64{-1.57, 0.88, -1.93}, []float64{-0.78, 1.49, 1.34}, []float64{0.06, 0.7, 0.24}, 0.02)
	example([]float64{-0.56, -0.59, -0.15}, []float64{-1.31, 0.04, 0.08}, []float64{0.29, 0.15, 0.56}, 0.04)
	example([]float64{-0.82, -0.85, -1.33}, []float64{-0.79, -0.02, -1.06}, []float64{0.48, 0.14, 0.37}, 0.025)
	example([]float64{-0.82, -0.85, -1.33}, []float64{-0.72, -0.59, -0.6}, []float64{0.48, 0.41, 0.10}, 0.025)
	example([]float64{-1.34, 0.02, 0.79}, []float64{-1.36, 0.44, 0.56}, []float64{0.58, 0.36, 0.07}, 0.025)
	example([]float64{0.53, 0.82, 1.28}, []float64{1.86, -0.93, -2.29}, []float64{0.67, 0.27, 0.06}, 0.05)
	example([]float64{0.53, 0.82, 1.28}, []float64{1.44, -0.25, -1.37}, []float64{0.36, 0.45, 0.18}, 0.06)
	example([]float64{0.76, -0.64, -0.66}, []float64{1.00, -1.12, -0.43}, []float64{0.33, 0.09, 0.58}, 0.05)
	example([]float64{0.53, 0.82, 1.28}, []float64{1.86, -0.93, -2.29}, []float64{0.67, 0.27, 0.06}, 0.06)
	example([]float64{0.50, 1.24, 0.56}, []float64{1.69, 0.69, -2.44}, []float64{0.11, 0.32, 0.57}, 0.06)
	example([]float64{-1.51, -0.19, -1.67}, []float64{-1.37, -0.90, -1.03}, []float64{0.21, 0.75, 0.04}, 0.05)
	example([]float64{0.90, 1.17, -0.73}, []float64{1.28, 0.97, -1.01}, []float64{0.34, 0.32, 0.34}, 0.09)
	example([]float64{0.90, 1.17, -0.73}, []float64{0.72, 0.73, -0.84}, []float64{0.52, 0.25, 0.22}, 0.11)
	example([]float64{0.15, -0.36, 1.21}, []float64{0.89, 0.60, -2.31}, []float64{0.91, 0.05, 0.04}, 0.04)
	example([]float64{1.54, -0.41, 1.31}, []float64{2.04, -0.65, 1.05}, []float64{0.26, 0.27, 0.47}, 0.10)
	example([]float64{1.54, -0.41, 1.31}, []float64{1.30, -0.08, -1.88}, []float64{0.08, 0.17, 0.75}, 0.16)
	example([]float64{1.54, -0.41, 1.31}, []float64{0.97, -1.67, -1.93}, []float64{0.43, 0.42, 0.15}, 0.20)
	example([]float64{4.07, 0.84, 1.63}, []float64{3.09, 0.17, 0.02}, []float64{0.37, 0.22, 0.41}, 0.25)
	example([]float64{4.07, 0.84, 1.63}, []float64{1.35, 0.08, -0.83}, []float64{0.45, 0.36, 0.19}, 0.40)
	example([]float64{4.07, 0.84, 1.63}, []float64{1.32, 0.22, 0.06}, []float64{0.36, 0.08, 0.55}, 0.25)
	example([]float64{4.49, 0.63, 0.34}, []float64{2.22, 0.16, -0.77}, []float64{0.06, 0.42, 0.51}, 0.25)
	example([]float64{4.49, 0.63, 0.34}, []float64{2.06, 0.49, 0.18}, []float64{0.38, 0.25, 0.38}, 0.45)
	example([]float64{1.19, 1.92, 2.87}, []float64{2.49, -0.38, 0.35}, []float64{0.12, 0.74, 0.14}, 0.20)
	example([]float64{1.19, 1.92, 2.87}, []float64{2.11, -1.59, 1.01}, []float64{0.20, 0.63, 0.18}, 0.20)
	//
	//
	loss := func() float64 {
		sum := 0.0
		for i := range ys {
			p := ws[i].Yes(ms[i])
			sum += (p - ys[i]) * (p - ys[i]) / (ys[i] + 0.001)
		}
		return sum / float64(len(ys))
	}

	spsa(w, loss, r, 0.001, 0.075, 100000)

	fmt.Printf("Final loss: %.5f\n", loss())

	return w
}

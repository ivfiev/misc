package main

import (
	"fmt"
	"math"
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

type Dataset struct {
	Rank []*Rank
	MSE  []*MSE
}

type (
	vec  []float64
	Rank struct {
		W0, W1 *W
		M0, M1 *M
	}
	MSE struct {
		W *W
		M *M
		y float64
	}
)

func NewDataset() *Dataset {
	return &Dataset{
		Rank: make([]*Rank, 0),
		MSE:  make([]*MSE, 0),
	}
}

func (d *Dataset) AddPoint(w *W, m *M, y float64) {
	d.MSE = append(d.MSE, &MSE{
		W: w,
		M: m,
		y: y,
	})
}

func (d *Dataset) AddGt(w0 *W, m0 *M, w1 *W, m1 *M) {
	d.Rank = append(d.Rank, &Rank{
		W0: w0,
		M0: m0,
		W1: w1,
		M1: m1,
	})
}

func (d *Dataset) LossMSE(f func(w *W, m *M) float64) float64 {
	sum := 0.0
	for _, m := range d.MSE {
		y := f(m.W, m.M)
		dy := (y - m.y)
		sum += dy * dy / (m.y + 0.001)
	}
	return sum / max(1, float64(len(d.MSE)))
}

func (d *Dataset) LossRank(f func(w *W, m *M) float64) float64 {
	sum := 0.0
	for _, r := range d.Rank {
		y0 := f(r.W0, r.M0)
		y1 := f(r.W1, r.M1)
		dy := y0 - y1
		sum += math.Log(1 + math.Exp(-dy))
	}
	return sum / max(1, float64(len(d.Rank)))
}

func trainWx(r *Random) []float64 {
	const n = 13
	w := r.Ns(n, 0, 0.025)
	data := NewDataset()

	point := func(mx, wx, wp []float64, y float64) {
		data.AddPoint(
			NewW(wx, wp, w, nil, nil),
			NewM(mx, nil),
			y,
		)
	}

	point(vec{0, 0, 0}, vec{0, 0, 0}, vec{0.333, 0.333, 0.333}, 0.03)
	point(vec{1, 0, 0}, vec{0, 0, 0}, vec{0.333, 0.333, 0.333}, 0.07)
	point(vec{1, 1, 1}, vec{0, 0, 0}, vec{0.333, 0.333, 0.333}, 0.10)

	point(vec{0, 0, 0}, vec{1, 0, 0}, vec{0.333, 0.333, 0.333}, 0.01)
	point(vec{0, 0, 0}, vec{2, 0, 0}, vec{0.333, 0.333, 0.333}, 0.005)
	point(vec{0, 0, 0}, vec{3, 0, 0}, vec{0.333, 0.333, 0.333}, 0.001)

	point(vec{0, 1, 0}, vec{1, 0, 0}, vec{0.333, 0.333, 0.333}, 0.02)
	point(vec{0, 2, 0}, vec{2, 0, 0}, vec{0.333, 0.333, 0.333}, 0.04)
	point(vec{0, 3, 0}, vec{3, 0, 0}, vec{0.333, 0.333, 0.333}, 0.07)

	point(vec{2, 0, 0}, vec{0, 0, 0}, vec{0.8, 0.1, 0.1}, 0.20)
	point(vec{0, 2, 0}, vec{0, 0, 0}, vec{0.8, 0.1, 0.1}, 0.05)
	point(vec{0, 0, 2}, vec{0, 0, 0}, vec{0.8, 0.1, 0.1}, 0.05)

	point(vec{2, 0, 0}, vec{0, 0, 0}, vec{0.1, 0.8, 0.1}, 0.075)
	point(vec{0, 2, 0}, vec{0, 0, 0}, vec{0.1, 0.8, 0.1}, 0.15)
	point(vec{0, 0, 2}, vec{0, 0, 0}, vec{0.1, 0.8, 0.1}, 0.05)

	point(vec{2, 0, 0}, vec{0, 0, 0}, vec{0.1, 0.1, 0.8}, 0.075)
	point(vec{0, 2, 0}, vec{0, 0, 0}, vec{0.1, 0.1, 0.8}, 0.12)
	point(vec{0, 0, 2}, vec{0, 0, 0}, vec{0.1, 0.1, 0.8}, 0.20)

	point(vec{4, 4, 4}, vec{0, 0, 0}, vec{0.1, 0.1, 0.8}, 0.50)
	point(vec{4, 4, 4}, vec{0, 0, 0}, vec{0.1, 0.8, 0.1}, 0.50)
	point(vec{4, 4, 4}, vec{0, 0, 0}, vec{0.8, 0.1, 0.1}, 0.50)

	point(vec{4, 4, 4}, vec{4, 4, 4}, vec{0.1, 0.1, 0.8}, 0.25)
	point(vec{4, 4, 4}, vec{4, 4, 4}, vec{0.1, 0.8, 0.1}, 0.25)
	point(vec{4, 4, 4}, vec{4, 4, 4}, vec{0.8, 0.1, 0.1}, 0.25)

	point(vec{-1, 2, -2}, vec{1, 1, 1}, vec{0.2, 0.3, 0.5}, 0.01)
	point(vec{-4, 2, 2}, vec{1, 1, 1}, vec{0.2, 0.3, 0.5}, 0.001)
	point(vec{4, -2, -2}, vec{2, 2, 2}, vec{0.4, 0.3, 0.3}, 0.10)
	point(vec{-1, 5, -1}, vec{0, 0, 0}, vec{0.05, 0.9, 0.05}, 0.50)

	point(vec{-3, 2, 2}, vec{0, 0, 0}, vec{0.2, 0.4, 0.4}, 0.02)
	point(vec{2, -3, 2}, vec{0, 0, 0}, vec{0.4, 0.2, 0.4}, 0.08)
	point(vec{2, 2, -3}, vec{0, 0, 0}, vec{0.4, 0.4, 0.2}, 0.08)

	point(vec{4, 0, 0}, vec{0, 0, 0}, vec{0.3, 0.4, 0.3}, 0.40)
	point(vec{2, 2, 2}, vec{0, 0, 0}, vec{0.3, 0.4, 0.3}, 0.25)

	point(vec{0, 1, 0}, vec{0, 0, 0}, vec{0.333, 0.333, 0.333}, 0.06)
	point(vec{0, 0, 1}, vec{0, 0, 0}, vec{0.333, 0.333, 0.333}, 0.06)
	point(vec{0, 0, 1}, vec{0, 0, 0}, vec{0.1, 0.1, 0.8}, 0.09)
	point(vec{1, -1, 1}, vec{0, 0, 0}, vec{0.4, 0.3, 0.3}, 0.08)

	point(vec{-1, 0, 0}, vec{3, 3, 3}, vec{0.4, 0.3, 0.3}, 0.0005)
	point(vec{0, -1, -1}, vec{1, 1, 1}, vec{0.4, 0.3, 0.3}, 0.005)
	point(vec{-1, 2, 1}, vec{2, -1, -1}, vec{0.05, 0.9, 0.05}, 0.15)
	point(vec{-1, 2, 1}, vec{0, -2, 0}, vec{0.01, 0.8, 0.19}, 0.18)
	point(vec{-1, 1, 2}, vec{2, -1, -1}, vec{0.05, 0.05, 0.9}, 0.15)
	point(vec{-1, 1, 2}, vec{0, -2, 0}, vec{0.01, 0.19, 0.8}, 0.18)

	point(vec{-0.74, 0.71, -0.36}, vec{-1.69, -1.55, -0.88}, vec{0.2, 0.31, 0.49}, 0.08)
	point(vec{-0.74, 0.71, -0.36}, vec{-1.82, -1.28, -1.25}, vec{0.19, 0.36, 0.45}, 0.10)
	point(vec{-0.52, 1.34, 0.57}, vec{-0.62, -3.07, 0.73}, vec{0.2, 0.3, 0.5}, 0.12)
	point(vec{-1.57, 0.88, -1.93}, vec{-0.62, -3.07, 0.73}, vec{0.2, 0.3, 0.5}, 0.02)
	point(vec{-1.57, 0.88, -1.93}, vec{-0.78, 1.49, 1.34}, vec{0.06, 0.7, 0.24}, 0.02)
	point(vec{-0.56, -0.59, -0.15}, vec{-1.31, 0.04, 0.08}, vec{0.29, 0.15, 0.56}, 0.04)
	point(vec{-0.82, -0.85, -1.33}, vec{-0.79, -0.02, -1.06}, vec{0.48, 0.14, 0.37}, 0.025)
	point(vec{-0.82, -0.85, -1.33}, vec{-0.72, -0.59, -0.6}, vec{0.48, 0.41, 0.10}, 0.025)
	point(vec{-1.34, 0.02, 0.79}, vec{-1.36, 0.44, 0.56}, vec{0.58, 0.36, 0.07}, 0.025)
	point(vec{0.53, 0.82, 1.28}, vec{1.86, -0.93, -2.29}, vec{0.67, 0.27, 0.06}, 0.05)
	point(vec{0.53, 0.82, 1.28}, vec{1.44, -0.25, -1.37}, vec{0.36, 0.45, 0.18}, 0.06)
	point(vec{0.76, -0.64, -0.66}, vec{1.00, -1.12, -0.43}, vec{0.33, 0.09, 0.58}, 0.05)
	point(vec{0.53, 0.82, 1.28}, vec{1.86, -0.93, -2.29}, vec{0.67, 0.27, 0.06}, 0.06)
	point(vec{0.50, 1.24, 0.56}, vec{1.69, 0.69, -2.44}, vec{0.11, 0.32, 0.57}, 0.06)
	point(vec{-1.51, -0.19, -1.67}, vec{-1.37, -0.90, -1.03}, vec{0.21, 0.75, 0.04}, 0.05)
	point(vec{0.90, 1.17, -0.73}, vec{1.28, 0.97, -1.01}, vec{0.34, 0.32, 0.34}, 0.09)
	point(vec{0.90, 1.17, -0.73}, vec{0.72, 0.73, -0.84}, vec{0.52, 0.25, 0.22}, 0.11)
	point(vec{0.15, -0.36, 1.21}, vec{0.89, 0.60, -2.31}, vec{0.91, 0.05, 0.04}, 0.04)
	point(vec{1.54, -0.41, 1.31}, vec{2.04, -0.65, 1.05}, vec{0.26, 0.27, 0.47}, 0.10)
	point(vec{1.54, -0.41, 1.31}, vec{1.30, -0.08, -1.88}, vec{0.08, 0.17, 0.75}, 0.16)
	point(vec{1.54, -0.41, 1.31}, vec{0.97, -1.67, -1.93}, vec{0.43, 0.42, 0.15}, 0.20)
	point(vec{4.07, 0.84, 1.63}, vec{3.09, 0.17, 0.02}, vec{0.37, 0.22, 0.41}, 0.25)
	point(vec{4.07, 0.84, 1.63}, vec{1.35, 0.08, -0.83}, vec{0.45, 0.36, 0.19}, 0.40)
	point(vec{4.07, 0.84, 1.63}, vec{1.32, 0.22, 0.06}, vec{0.36, 0.08, 0.55}, 0.25)
	point(vec{4.49, 0.63, 0.34}, vec{2.22, 0.16, -0.77}, vec{0.06, 0.42, 0.51}, 0.25)
	point(vec{4.49, 0.63, 0.34}, vec{2.06, 0.49, 0.18}, vec{0.38, 0.25, 0.38}, 0.45)
	point(vec{1.19, 1.92, 2.87}, vec{2.49, -0.38, 0.35}, vec{0.12, 0.74, 0.14}, 0.20)
	point(vec{1.19, 1.92, 2.87}, vec{2.11, -1.59, 1.01}, vec{0.20, 0.63, 0.18}, 0.20)
	//
	//
	loss := func() float64 {
		return data.LossMSE(func(w *W, m *M) float64 {
			return w.Covfefe(m)
		})
	}

	spsa(w, loss, r, 0.0001, 0.01, 10000)

	fmt.Printf("Wx loss: %.5f\n", loss())

	return w
}

func trainWy(r *Random) []float64 {
	const n = 11
	wy := r.Ns(n, 0, 0.025)
	data := NewDataset()

	gt := func(w0y, m0y, w1y, m1y vec) {
		data.AddGt(
			NewW(nil, nil, nil, w0y, wy),
			NewM(nil, m0y),
			NewW(nil, nil, nil, w1y, wy),
			NewM(nil, m1y),
		)
	}

	point := func(w0y, m0y vec, y float64) {
		data.AddPoint(NewW(nil, nil, nil, w0y, wy), NewM(nil, m0y), y)
	}

	point(vec{-1, 0}, vec{1, 0}, 0.10)
	point(vec{0, 0}, vec{0, 0}, 0.25)
	point(vec{0, 1}, vec{0, 1}, 0.35)
	point(vec{0, -1}, vec{0, -1}, 0.30)

	gt(vec{0, 0}, vec{0, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{-1, 0}, vec{1, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{0, 0}, vec{0, 0}, vec{2, 0}, vec{-2, 0})
	gt(vec{0, 0}, vec{0, 0}, vec{1, 0}, vec{-1, 0})
	gt(vec{2, 0}, vec{2, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{1, 0}, vec{1, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{1, 0}, vec{1, 0}, vec{-1, 0}, vec{1, 0})
	gt(vec{-2, 0}, vec{-2, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{-2, 0}, vec{-2, 0}, vec{-1, 0}, vec{1, 0})
	gt(vec{0, 2}, vec{0, 2}, vec{0, 0}, vec{0, 0})
	gt(vec{0, 2}, vec{0, 2}, vec{0, -1}, vec{0, 1})
	gt(vec{0, 2}, vec{0, 2}, vec{0, 1}, vec{0, -1})
	gt(vec{0, 0}, vec{0, 0}, vec{0, 2}, vec{0, -2})
	gt(vec{-2, 0}, vec{2, 0}, vec{-2, 2}, vec{2, -2})
	gt(vec{2, 0}, vec{-2, 0}, vec{-2, 0}, vec{2, 0})
	gt(vec{2, -2}, vec{2, -2}, vec{-2, -2}, vec{-2, -2})
	gt(vec{-2, 2}, vec{2, 2}, vec{-2, 0}, vec{2, 0})
	gt(vec{-2, 2}, vec{2, 2}, vec{2, 0}, vec{-2, 0})
	gt(vec{-1, 2}, vec{1, 2}, vec{1, 0}, vec{-1, 0})
	gt(vec{1, 0}, vec{0, 0}, vec{-1, 0}, vec{0, 0})
	gt(vec{1, 0}, vec{0, -2}, vec{-1, 0}, vec{0, -2})
	gt(vec{-0.5, 1}, vec{-1, 2}, vec{-0.5, 1}, vec{-1, 2})

	loss := func() float64 {
		mse := data.LossMSE(func(w *W, m *M) float64 {
			_, p := w.LTR(m)
			return p
		})
		rank := data.LossRank(func(w *W, m *M) float64 {
			u, _ := w.LTR(m)
			return u
		})
		return mse + rank
	}

	spsa(wy, loss, r, 0.001, 0.25, 20000)

	fmt.Printf("Wy loss: %.5f\n", loss())

	return wy
}

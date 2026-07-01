package main

import (
	"math"
	"math/rand"
)

type Random struct {
	rng *rand.Rand
}

func NewRandom(seed int64) *Random {
	return &Random{rand.New(rand.NewSource(seed))}
}

func (r *Random) N(u, o float64) float64 {
	return u + o*r.rng.NormFloat64()
}

func (r *Random) Ns(n int, u, o float64) []float64 {
	v := make([]float64, n)
	for i := range n {
		v[i] = r.N(u, o)
	}
	return v
}

func (r *Random) Ix(n int) int {
	return r.rng.Int() % n
}

func (r *Random) U(a, b float64) float64 {
	return a + b*r.rng.Float64()
}

type Logistic struct {
	L, k, x0 float64
}

func FitLogistic(L, xm, ym, xg, yg float64) *Logistic {
	kg := (math.Log(L/yg-1) - math.Log(L/ym-1)) / (xm - xg)
	xg = math.Log(L/ym-1)/kg + xm
	return &Logistic{L, kg, xg}
}

func (l *Logistic) Y(x float64) float64 {
	return l.L / (1 + math.Exp(-l.k*(x-l.x0)))
}

func softmax(v []float64) {
	sum := 0.0
	for _, x := range v {
		sum += math.Exp(x) // don't bother -max
	}
	for i := range v {
		v[i] = math.Exp(v[i]) / sum
	}
}

func clamp(a, x, b float64) float64 {
	return max(a, min(x, b))
}

func sigmoid(x float64) float64 {
	return 1 / (1 + math.Exp(-x))
}

func pos(x float64) float64 {
	if x < 0 {
		return 0
	}
	return x
}

func neg(x float64) float64 {
	if x > 0 {
		return 0
	}
	return x
}

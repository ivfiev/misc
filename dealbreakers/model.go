package main

import (
	"fmt"
	"math"
	"strings"
)

const ATTRS = 3

type M struct {
	X []float64
	W []*W
}

func RandomM(r *Random) *M {
	return &M{r.Ns(ATTRS, 0, 1), make([]*W, 0)}
}

func (m *M) Day(ws []*W, r *Random) {
	w := ws[r.Ix(len(ws))]
	if r.U(0, 1) < w.Yes(m) {
		m.W = append(m.W, w)
	}
}

func (m *M) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "X: %s\n", fmtv(m.X))
	if len(m.W) > 0 {
		for _, w := range m.W {
			fmt.Fprintf(&b, "  %s, p=%.2f\n", w.String(), w.Yes(m))
		}
	}
	return b.String()
}

type W struct {
	X  []float64
	P  []float64
	Sx []*Logistic
	A  []float64
	Sa *Logistic
	D  float64
	I  float64
	B  float64
}

func RandomW(r *Random) *W {
	w := &W{
		X: r.Ns(ATTRS, 0, 1),
		P: r.Ns(ATTRS, 0, 1),
		A: []float64{clamp(0.01, r.N(0.1, 0.05), 0.25), clamp(0.333, r.N(0.5, 0.1), 0.999)},
		D: clamp(0, r.N(0.1, 0.02), 0.2),
		I: clamp(0, r.N(0.25, 0.05), 0.5),
		B: clamp(0, r.N(0.1, 0.02), 0.2),
	}
	softmax(w.P)
	w.fit()
	return w
}

func NewW(x, p, a []float64, d, i, b float64) *W {
	w := &W{
		X: x,
		P: p,
		A: a,
		D: d,
		I: i,
		B: b,
	}
	w.fit()
	return w
}

func (w *W) fit() {
	w.Sx = []*Logistic{
		FitLogistic3(1, -1, 0.05, 0, 0.2, 1, 0.4),
		FitLogistic3(1, -1, 0.08, 0, 0.333, 1, 0.5),
		FitLogistic3(1, -1, 0.08, 0, 0.333, 1, 0.5),
	}
	w.Sa = FitLogistic2(1, 0.25, w.A[0], 0.999, w.A[1])
}

func (w *W) Yes(m *M) float64 {
	sum := 0.0
	for i := range w.X {
		d := m.X[i] - w.X[i]
		penalty := w.D * math.Pow(min(0, d), 2)
		inflation := w.I * max(0, w.X[i])
		bonus := w.B * math.Pow(max(0, d), 2)
		sum += w.Sx[i].Y(m.X[i]-penalty-inflation+bonus) * w.P[i]
	}
	return w.Sa.Y(sum) * sum
}

func (w *W) String() string {
	return fmt.Sprintf("X: %s, P: %s, A: %s, DIB: %s", fmtv(w.X), fmtv(w.P), fmtv(w.A), fmtv([]float64{w.D, w.I, w.B}))
}

func fmtv(v []float64) string {
	var b strings.Builder
	b.WriteRune('[')
	for i, x := range v {
		fmt.Fprintf(&b, "%.2f", x)
		if i < len(v)-1 {
			b.WriteString(", ")
		}
	}
	b.WriteRune(']')
	return b.String()
}

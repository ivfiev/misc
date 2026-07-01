package main

import (
	"fmt"
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

func NewM(x []float64) *M {
	return &M{X: x, W: make([]*W, 0)}
}

func (m *M) Day(ws []*W, r *Random) {
	const k = 10
	var c *W
	for range k {
		w := ws[r.Ix(len(ws))]
		if c == nil || w.X[0] > c.X[0] {
			c = w
		}
	}
	if r.U(0, 1) < c.Yes(m) {
		m.W = append(m.W, c)
	}
}

func (m *M) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "X: %s\n", fmtv(m.X))
	if len(m.W) > 0 {
		for _, w := range m.W {
			fmt.Fprintf(&b, "  %s, p=%.2f\n", w.String(), w.Yes(m))
			// fmt.Fprintf(&b, "  example([]float64{%.2f, %.2f, %.2f}, []float64{%.2f, %.2f, %.2f}, []float64{%.2f, %.2f, %.2f}, )\n", m.X[0], m.X[1], m.X[2], w.X[0], w.X[1], w.X[2], w.P[0], w.P[1], w.P[2])
		}
	}
	return b.String()
}

type W struct {
	X []float64
	P []float64
	W []float64
	A *Logistic
}

func (w *W) Yes(m *M) float64 {
	d0, d1, d2 := w.X[0]-m.X[0], w.X[1]-m.X[1], w.X[2]-m.X[2]
	p0, p1, p2 := w.P[0], w.P[1], w.P[2]
	m0, m1, m2 := m.X[0], m.X[1], m.X[2]

	i := 0
	t := func() float64 {
		w := w.W[i]
		i++
		return w
	}

	e := t()*d0*p0 + t()*d1*p1 + t()*d2*p2
	e += t()*pos(d0)*pos(d1) + t()*pos(d0)*pos(d2) + t()*pos(d1)*pos(d2)
	e += t()*neg(d0)*neg(d1) + t()*neg(d0)*neg(d2) + t()*neg(d1)*neg(d2)
	e += t()*m0*p0 + t()*m1*p1 + t()*m2*p2
	e += t()
	e = sigmoid(e)
	return w.A.Y(e) * e
}

func RandomW(r *Random, ws []float64) *W {
	w := NewW(
		r.Ns(ATTRS, 0, 1),
		r.Ns(ATTRS, 0, 1),
		ws,
	)
	softmax(w.P)
	return w
}

func NewW(x, p, w []float64) *W {
	return &W{
		X: x,
		P: p,
		W: w,
		A: FitLogistic(1, 0.25, 0.10, 1.0, 0.50),
	}
}

func (w *W) String() string {
	return fmt.Sprintf("X: %s, P: %s", fmtv(w.X), fmtv(w.P))
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

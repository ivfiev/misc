package main

import (
	"fmt"
	"strings"
)

const (
	ATTRSX = 3
	ATTRSY = 2
)

type M struct {
	X []float64
	Y []float64
	W []*W
}

func RandomM(r *Random) *M {
	return &M{r.Ns(ATTRSX, 0, 1), r.Ns(ATTRSY, 0, 1), make([]*W, 0)}
}

func NewM(x, y []float64) *M {
	return &M{X: x, Y: y, W: make([]*W, 0)}
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
	if r.U(0, 1) < c.Covfefe(m) {
		m.W = append(m.W, c)
	}
}

func (m *M) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "X: %s, Y: %s\n", fmtv(m.X), fmtv(m.Y))
	if len(m.W) > 0 {
		for _, w := range m.W {
			p0 := w.Covfefe(m)
			_, p1 := w.LTR(m)
			fmt.Fprintf(&b, "  %s, p0=%.2f, p1=%.2f\n", w.String(), p0, p1)
			// fmt.Fprintf(&b, "  example([]float64{%.2f, %.2f, %.2f}, []float64{%.2f, %.2f, %.2f}, []float64{%.2f, %.2f, %.2f}, )\n", m.X[0], m.X[1], m.X[2], w.X[0], w.X[1], w.X[2], w.P[0], w.P[1], w.P[2])
		}
	}
	return b.String()
}

type W struct {
	X  []float64
	Px []float64
	Wx []float64
	A  *Logistic

	Y  []float64
	Wy []float64
}

func (w *W) Covfefe(m *M) float64 {
	p0, p1, p2 := w.Px[0], w.Px[1], w.Px[2]
	m0, m1, m2 := m.X[0], m.X[1], m.X[2]
	w0, w1, w2 := w.X[0], w.X[1], w.X[2]
	d0, d1, d2 := w0-m0, w1-m1, w2-m2

	i := 0
	t := func() float64 {
		w := w.Wx[i]
		i++
		return w
	}

	E := t()*d0*p0 + t()*d1*p1 + t()*d2*p2
	E += t()*pos(d0)*pos(d1) + t()*pos(d0)*pos(d2) + t()*pos(d1)*pos(d2)
	E += t()*neg(d0)*neg(d1) + t()*neg(d0)*neg(d2) + t()*neg(d1)*neg(d2)
	E += t()*m0*p0 + t()*m1*p1 + t()*m2*p2
	E += t()
	E = sigmoid(E)
	A := w.A.Y(E)
	return A * E
}

func (w *W) LTR(m *M) (float64, float64) {
	const T = 10.0
	i := 0
	t := func() float64 {
		w := w.Wy[i]
		i++
		return w
	}
	U := 0.0
	for i := range w.Y {
		wy, my := w.Y[i], m.Y[i]
		U += t() * (wy - my)
		U += t() * pos(wy) * pos(my)
		U += t() * neg(wy) * neg(my)
		U += t() * pos(wy) * neg(my)
		U += t() * neg(wy) * pos(my)
	}
	U += t()
	P := sigmoid(U / T)
	return U, P
}

func RandomW(r *Random, wx, wy []float64) *W {
	w := NewW(
		r.Ns(ATTRSX, 0, 1),
		r.Ns(ATTRSX, 0, 1),
		wx,
		r.Ns(ATTRSY, 0, 1),
		wy,
	)
	softmax(w.Px)
	return w
}

func NewW(x, p, wx, y, wy []float64) *W {
	return &W{
		X:  x,
		Px: p,
		Wx: wx,
		A:  FitLogistic(1, 0.25, 0.10, 1.0, 0.50),
		Y:  y,
		Wy: wy,
	}
}

func (w *W) String() string {
	return fmt.Sprintf("X: %s, P: %s, Y: %s", fmtv(w.X), "-", fmtv(w.Y))
	// return fmt.Sprintf("X: %s, P: %s, Y: %s", fmtv(w.X), fmtv(w.Px), fmtv(w.Y))
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

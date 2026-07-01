package main

import (
	"fmt"
	"math"
	"math/rand"
	"slices"
	"strings"
	"time"
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

type Logistic struct { // piecewise
	L, kl, kg, xl, xg float64
}

func FitLogistic3(L, xl, yl, xm, ym, xg, yg float64) *Logistic {
	kg := (math.Log(L/yg-1) - math.Log(L/ym-1)) / (xm - xg)
	kl := (math.Log(L/yl-1) - math.Log(L/ym-1)) / (xm - xl)
	xg = math.Log(L/ym-1)/kg + xm
	xl = math.Log(L/ym-1)/kl + xm
	return &Logistic{L, kl, kg, xl, xg}
}

func FitLogistic2(L, xm, ym, xg, yg float64) *Logistic {
	return FitLogistic3(L, xg, yg, xm, ym, xg, yg)
}

func (l *Logistic) Y(x float64) float64 {
	if x < 0 {
		return l.L / (1 + math.Exp(-l.kl*(x-l.xl)))
	}
	return l.L / (1 + math.Exp(-l.kg*(x-l.xg)))
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
	// test()
	sim()
	// f := FitLogistic(1, -1, 0.03, 0, 0.2, 1, 0.4)
	// println(f.Y(0))
	// println(f.Y(1))
	// println(f.Y(-1))
}

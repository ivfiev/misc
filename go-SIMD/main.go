package main

import (
	"fmt"
	"math"
	"math/bits"
	"math/rand/v2"
	"simd/archsimd"
	"slices"
	"time"
)

const (
	n = 1024
	d = 1024
)

func randVec() []float32 {
	v := make([]float32, d)
	for i := range d {
		v[i] = float32(rand.NormFloat64() / d)
	}
	return v
}

func calcError(want, got any) float64 {
	switch want.(type) {
	case float32:
		want, got := want.(float32), got.(float32)
		return math.Abs(float64(want - got))
	case float64:
		want, got := want.(float64), got.(float64)
		return math.Abs(want - got)
	case []float32:
		want, got := want.([]float32), got.([]float32)
		err := 0.0
		for i := range want {
			err = max(err, math.Abs(float64((want[i]-got[i])/want[i])))
		}
		return err
	case []float64:
		want, got := want.([]float64), got.([]float64)
		err := 0.0
		for i := range want {
			err = max(err, math.Abs(float64((want[i]-got[i])/want[i])))
		}
		return err
	case [][]float32:
		want, got := want.([][]float32), got.([][]float32)
		err := 0.0
		for i := range want {
			err = max(err, calcError(want[i], got[i]))
		}
		return err
	case [][]float64:
		want, got := want.([][]float64), got.([][]float64)
		err := 0.0
		for i := range want {
			err = max(err, calcError(want[i], got[i]))
		}
		return err
	case int:
		want, got := want.(int), got.(int)
		return math.Abs(float64(want - got))
	default:
		return -1.0
	}
}

func measure(f, g func() (string, any)) {
	t0 := time.Now()
	labelf, fs := f()
	t1 := time.Now()
	labelg, gs := g()
	t2 := time.Now()
	fmt.Printf("%s: %dms\n%s: %dms\nerr: %.6f\n",
		labelf, t1.Sub(t0).Milliseconds(),
		labelg, t2.Sub(t1).Milliseconds(),
		calcError(fs, gs))
}

func dotScalar(u, v []float32) float32 {
	var sum float32
	for i := range u {
		sum += u[i] * v[i]
	}
	return sum
}

func dotSimd(u, v []float32) float32 {
	s0 := archsimd.BroadcastFloat32x16(0.0)
	s1 := archsimd.BroadcastFloat32x16(0.0)
	for i := 0; i < len(u)-32 && i < len(v)-32; i += 32 {
		s0 = s0.Add(archsimd.LoadFloat32x16(u[i:]).Mul(archsimd.LoadFloat32x16(v[i:])))
		s1 = s1.Add(archsimd.LoadFloat32x16(u[i+16:]).Mul(archsimd.LoadFloat32x16(v[i+16:])))
	}
	var xs [16]float32
	s0.Add(s1).StoreArray(&xs)
	var sum float32
	for i := range xs {
		sum += xs[i]
	}
	return sum
}

func dotProds() {
	vs := [][]float32{}
	for range n {
		vs = append(vs, randVec())
	}
	var sum float32
	measure(
		func() (string, any) {
			sum = 0.0
			for i := range n {
				for j := range n {
					sum += dotScalar(vs[i], vs[j])
				}
			}
			return "dot scalar", sum
		},
		func() (string, any) {
			sum = 0.0
			for i := range n {
				for j := range n {
					sum += dotSimd(vs[i], vs[j])
				}
			}
			return "dot SIMD", sum
		},
	)
}

func strlenScalar(s []uint8) int {
	for i := range s {
		if s[i] == 0 {
			return i
		}
	}
	return -1
}

func strlenSimd(s []uint8) int {
	target := archsimd.BroadcastUint8x64(0)
	for i := 0; i < len(s); {
		x, di := archsimd.LoadUint8x64Part(s[i:])
		y := x.Equal(target)
		b := y.ToBits()
		if b > 0 {
			return i + bits.TrailingZeros(uint(b))
		}
		i += di
	}
	return -1
}

func strlens() {
	const (
		n = 100000
		k = 100000
	)
	bytes := make([]uint8, n)
	for i := range bytes {
		bytes[i] = 1 + uint8(rand.Int()%255)
	}
	tests := make([]int, k)
	for i := range tests {
		tests[i] = rand.Int() % n
	}
	var sum int
	measure(
		func() (string, any) {
			sum = 0
			for i := range tests {
				tmp := bytes[tests[i]]
				bytes[tests[i]] = 0
				sum += strlenScalar(bytes)
				bytes[tests[i]] = tmp
			}
			return "strlen scalar", sum
		},
		func() (string, any) {
			sum = 0
			for i := range tests {
				tmp := bytes[tests[i]]
				bytes[tests[i]] = 0
				sum += strlenSimd(bytes)
				bytes[tests[i]] = tmp
			}
			return "strlen SIMD", sum
		},
	)
}

func countScalar(bytes []uint8, b uint8) int {
	sum := 0
	for i := range bytes {
		if bytes[i] == b {
			sum++
		}
	}
	return sum
}

func countSimd(bytes []uint8, b uint8) int {
	sum := 0
	m := archsimd.BroadcastUint8x64(b)
	for i := 0; i < len(bytes); {
		a, di := archsimd.LoadUint8x64Part(bytes[i:])
		b := uint(a.Equal(m).ToBits())
		sum += bits.OnesCount(b)
		i += di
	}
	return sum
}

func counts() {
	bs := make([]uint8, 10000000)
	for i := range bs {
		bs[i] = uint8(rand.Int() % 256)
	}
	measure(
		func() (string, any) {
			sum := 0
			for b := 1; b <= 255; b++ {
				sum += countScalar(bs, uint8(b))
			}
			return "count scalar", sum
		},
		func() (string, any) {
			sum := 0
			for b := 1; b <= 255; b++ {
				sum += countSimd(bs, uint8(b))
			}
			return "count SIMD", sum
		},
	)
}

func softmaxScalar(input, output []float64) {
	m := slices.Max(input)
	sum := 0.0
	for i := range output {
		output[i] = input[i] - m
		sum += math.Exp(output[i])
	}
	for i := range output {
		output[i] = math.Exp(output[i]) / sum
	}
}

func expSimd8x64(a, b []float64) {
	const (
		ln2    float64 = 0.6931471805599453
		ln2inv float64 = 1.4426950408889634
	)
	// for i := range a {
	// 	// exp(x) = exp(n * ln(2) + r) = 2^n * exp(r)
	// 	n := int32(math.Round(float64(a[i] * ln2inv)))
	// 	r := a[i] - float32(n)*ln2
	// 	f := math.Float32frombits(uint32(127+n) << 23)
	// 	// r2 := r * r
	// 	// p := 1 + r + r2*(1.0/2.0) + r*r2*(1.0/6.0) + r2*r2*(1.0/24.0)
	// 	g := 1 + r*(1.0+r*(1.0/2.0+r*(1.0/6.0+r*(1.0/24.0))))
	// 	b[i] = f * g
	// }
	bln2 := archsimd.BroadcastFloat64x8(ln2)
	bln2inv := archsimd.BroadcastFloat64x8(ln2inv)
	_1 := archsimd.BroadcastFloat64x8(1)
	_1_2 := archsimd.BroadcastFloat64x8(1.0 / 2.0)
	_1_6 := archsimd.BroadcastFloat64x8(1.0 / 6.0)
	_1_24 := archsimd.BroadcastFloat64x8(1.0 / 24.0)
	_1_120 := archsimd.BroadcastFloat64x8(1.0 / 120.0)
	_1_720 := archsimd.BroadcastFloat64x8(1.0 / 720.0)
	for i := 0; i < len(a); {
		xs, di := archsimd.LoadFloat64x8Part(a[i:])
		ns := xs.Mul(bln2inv).RoundScaled(0)
		rs := xs.Sub(ns.Mul(bln2))
		fs := _1.Scale(ns)
		ps := _1_720
		ps = ps.MulAdd(rs, _1_120)
		ps = ps.MulAdd(rs, _1_24)
		ps = ps.MulAdd(rs, _1_6)
		ps = ps.MulAdd(rs, _1_2)
		ps = ps.MulAdd(rs, _1)
		ps = ps.MulAdd(rs, _1)
		fs.Mul(ps).StorePart(b[i:])
		i += di
	}
}

func softmaxSIMD(input, output []float64) { // ass
	reg := archsimd.BroadcastFloat64x8(-99999)
	var buf [8]float64
	for i := 0; i < len(output); {
		o, di := archsimd.LoadFloat64x8Part(input[i:]) // max becomes 0 if all are negative... unlikely tho
		reg = reg.Max(o)
		i += di
	}
	reg.StoreArray(&buf)
	var m float64 = -99999999
	for i := range buf {
		if buf[i] > m {
			m = buf[i]
		}
	}
	reg = archsimd.BroadcastFloat64x8(m)
	for i := 0; i < len(output); {
		o, di := archsimd.LoadFloat64x8Part(input[i:])
		o = o.Sub(reg)
		o.StorePart(output[i:])
		i += di
	}
	expSimd8x64(output, output)
	sum := archsimd.BroadcastFloat64x8(0)
	for i := 0; i < len(output); {
		o, di := archsimd.LoadFloat64x8Part(output[i:])
		sum = sum.Add(o)
		i += di
	}
	var s float64 = 0
	sum.StoreArray(&buf)
	for i := range buf {
		s += buf[i]
	}
	sum = archsimd.BroadcastFloat64x8(s)
	recip := sum.Reciprocal()
	for i := 0; i < len(output); {
		o, di := archsimd.LoadFloat64x8Part(output[i:])
		o = o.Mul(recip)
		o.StorePart(output[i:])
		i += di
	}
}

func softmaxes() {
	vecs := make([][]float64, 100)
	for i := range vecs {
		vecs[i] = make([]float64, 100000)
		for range len(vecs[i]) {
			vecs[i] = append(vecs[i], rand.NormFloat64())
		}
	}
	output1 := make([]float64, len(vecs[0]))
	output2 := make([]float64, len(vecs[0]))
	measure(
		func() (string, any) {
			for _, v := range vecs {
				softmaxScalar(v, output1)
			}
			return "scalar softmax", output1
		},
		func() (string, any) {
			for _, v := range vecs {
				softmaxSIMD(v, output2)
			}
			return "SIMD softmax", output2
		},
	)
}

func expScalar(a, b []float32) {
	for i := range a {
		b[i] = float32(math.Exp(float64(a[i])))
	}
}

func expSimd(a, b []float32) {
	const (
		ln2    float32 = 0.6931471805599453
		ln2inv float32 = 1.4426950408889634
	)
	// for i := range a {
	// 	// exp(x) = exp(n * ln(2) + r) = 2^n * exp(r)
	// 	n := int32(math.Round(float64(a[i] * ln2inv)))
	// 	r := a[i] - float32(n)*ln2
	// 	f := math.Float32frombits(uint32(127+n) << 23)
	// 	// r2 := r * r
	// 	// p := 1 + r + r2*(1.0/2.0) + r*r2*(1.0/6.0) + r2*r2*(1.0/24.0)
	// 	g := 1 + r*(1.0+r*(1.0/2.0+r*(1.0/6.0+r*(1.0/24.0))))
	// 	b[i] = f * g
	// }
	bln2 := archsimd.BroadcastFloat32x16(ln2)
	bln2inv := archsimd.BroadcastFloat32x16(ln2inv)
	_1 := archsimd.BroadcastFloat32x16(1)
	_1_2 := archsimd.BroadcastFloat32x16(1.0 / 2.0)
	_1_6 := archsimd.BroadcastFloat32x16(1.0 / 6.0)
	_1_24 := archsimd.BroadcastFloat32x16(1.0 / 24.0)
	_1_120 := archsimd.BroadcastFloat32x16(1.0 / 120.0)
	_1_720 := archsimd.BroadcastFloat32x16(1.0 / 720.0)
	for i := 0; i < len(a); {
		xs, di := archsimd.LoadFloat32x16Part(a[i:])
		ns := xs.Mul(bln2inv).RoundScaled(0)
		rs := xs.Sub(ns.Mul(bln2))
		fs := _1.Scale(ns)
		ps := _1_720
		ps = ps.MulAdd(rs, _1_120)
		ps = ps.MulAdd(rs, _1_24)
		ps = ps.MulAdd(rs, _1_6)
		ps = ps.MulAdd(rs, _1_2)
		ps = ps.MulAdd(rs, _1)
		ps = ps.MulAdd(rs, _1)
		fs.Mul(ps).StorePart(b[i:])
		i += di
	}
}

func exps() {
	vecs := make([][]float32, 200)
	for i := range vecs {
		vecs[i] = make([]float32, 100000)
		for range len(vecs[i]) {
			vecs[i] = append(vecs[i], float32(rand.NormFloat64()*10.0))
		}
	}
	output1 := make([]float32, len(vecs[0]))
	output2 := make([]float32, len(vecs[0]))
	measure(
		func() (string, any) {
			for _, v := range vecs {
				expScalar(v, output1)
			}
			return "scalar exp", output1
		},
		func() (string, any) {
			for _, v := range vecs {
				expSimd(v, output2)
			}
			return "SIMD exp", output2
		},
	)
}

func matprodScalar(A, B, C [][]float32) {
	for i := range A {
		for j := range A[i] {
			C[i][j] = 0.0
			for k := range B {
				C[i][j] += A[i][k] * B[k][j]
			}
		}
	}
}

func matprodSIMD(A, B, C [][]float32) {
	bvec := make([]float32, len(B))
	var buf [16]float32
	for b := range B {
		for i := range B {
			bvec[i] = B[i][b]
		}
		for a := range A {
			acc := archsimd.BroadcastFloat32x16(0.0)
			for i := 0; i < len(A); {
				as, di := archsimd.LoadFloat32x16Part(A[a][i:])
				bs, _ := archsimd.LoadFloat32x16Part(bvec[i:])
				acc = as.MulAdd(bs, acc)
				i += di
			}
			acc.StoreArray(&buf)
			C[a][b] = 0.0
			for i := range buf {
				C[a][b] += buf[i]
			}
		}
	}
}

func matprods() {
	const d = 250
	const n = 1
	a := make([][]float32, d)
	b := make([][]float32, d)
	c1 := make([][]float32, d)
	c2 := make([][]float32, d)
	for i := range d {
		a[i] = make([]float32, d)
		b[i] = make([]float32, d)
		c1[i] = make([]float32, d)
		c2[i] = make([]float32, d)
		for j := range d {
			a[i][j] = float32(rand.NormFloat64() / math.Sqrt(d))
			b[i][j] = float32(rand.NormFloat64() / math.Sqrt(d))
		}
	}
	measure(
		func() (string, any) {
			for range n {
				matprodScalar(a, b, c1)
			}
			return "matprod scalar", c1
		},
		func() (string, any) {
			for range n {
				matprodSIMD(a, b, c2)
			}
			return "matprod simd", c2
		},
	)
	// a := [][]float32{
	// 	{1, 2},
	// 	{3, 4},
	// }
	// b := [][]float32{
	// 	{2, 3},
	// 	{4, 5},
	// }
	// c := [][]float32{{0, 0}, {0, 0}}
	// matprodScalar(a, b, c)
	// fmt.Printf("%v\n", c)
	// c = [][]float32{{0, 0}, {0, 0}}
	// matprodSIMD(a, b, c)
	// fmt.Printf("%v\n", c)
}

func main() {
	// dotProds()
	// strlens()
	// counts()
	// softmaxes()
	// matprods()
	exps()
}

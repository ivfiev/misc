package main

import (
	"fmt"
	"math/bits"
	"math/rand/v2"
	"simd/archsimd"
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

func measure(f, g func() string) {
	t0 := time.Now()
	labelf := f()
	t1 := time.Now()
	labelg := g()
	t2 := time.Now()
	fmt.Printf("%s: %dms\n%s: %dms\n", labelf, t1.Sub(t0).Milliseconds(), labelg, t2.Sub(t1).Milliseconds())
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
		func() string {
			sum = 0.0
			for i := range n {
				for j := range n {
					sum += dotScalar(vs[i], vs[j])
				}
			}
			fmt.Printf("%.9f\n", sum)
			return "dot scalar"
		},
		func() string {
			sum = 0.0
			for i := range n {
				for j := range n {
					sum += dotSimd(vs[i], vs[j])
				}
			}
			fmt.Printf("%.9f\n", sum)
			return "dot SIMD"
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
		func() string {
			sum = 0
			for i := range tests {
				tmp := bytes[tests[i]]
				bytes[tests[i]] = 0
				sum += strlenScalar(bytes)
				bytes[tests[i]] = tmp
			}
			println(sum)
			return "strlen scalar"
		},
		func() string {
			sum = 0
			for i := range tests {
				tmp := bytes[tests[i]]
				bytes[tests[i]] = 0
				sum += strlenSimd(bytes)
				bytes[tests[i]] = tmp
			}
			println(sum)
			return "strlen SIMD"
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
		func() string {
			sum := 0
			for b := 1; b <= 255; b++ {
				sum += countScalar(bs, uint8(b))
			}
			println(sum)
			return "count scalar"
		},
		func() string {
			sum := 0
			for b := 1; b <= 255; b++ {
				sum += countSimd(bs, uint8(b))
			}
			println(sum)
			return "count SIMD"
		},
	)
}

func main() {
	// dotProds()
	// strlens()
	// counts()
}

package main

import (
	"fmt"
	"math/rand"
	"simd/archsimd"
	"sync"
	"testing"

	"gonum.org/v1/gonum/mat"
)

const N = 256

func naive64(A, B, C [][]float64) {
	for i := range A {
		for j := range B[0] {
			C[i][j] = 0
			for k := range A[i] {
				C[i][j] += A[i][k] * B[k][j]
			}
		}
	}
}

func mm64x8(A, B, C [][]float64) {
	if len(A[0]) != len(B) {
		panic(fmt.Sprintf("A/B incompatible, %d != %d", len(A), len(B[0])))
	}
	if len(C) != len(A) || len(C[0]) != len(B[0]) {
		panic(fmt.Sprintf("C/A/B incompatible, %d %d %d %d", len(C), len(C[0]), len(A), len(B[0])))
	}
	buf := make([]float64, len(B))
	for b := range B[0] {
		for i := range B {
			buf[i] = B[i][b]
		}
		for a := range A {
			_acc := archsimd.BroadcastFloat64x8(0.0)
			for i := 0; i < len(A[a]); {
				_as, di := archsimd.LoadFloat64x8Part(A[a][i:])
				_bs, _ := archsimd.LoadFloat64x8Part(buf[i:])
				_acc = _as.MulAdd(_bs, _acc)
				i += di
			}
			_hi4, _lo4 := _acc.GetHi(), _acc.GetLo()
			_hi4 = _hi4.Add(_lo4)
			_hi2, _lo2 := _hi4.GetHi(), _hi4.GetLo()
			_hi2 = _hi2.Add(_lo2)
			C[a][b] = _hi2.GetElem(0) + _hi2.GetElem(1)
		}
	}
}

func mm32x16(A, B, C [][]float32) {
	if len(A[0]) != len(B) {
		panic(fmt.Sprintf("A/B incompatible, %d != %d", len(A), len(B[0])))
	}
	if len(C) != len(A) || len(C[0]) != len(B[0]) {
		panic(fmt.Sprintf("C/A/B incompatible, %d %d %d %d", len(C), len(C[0]), len(A), len(B[0])))
	}
	buf := make([]float32, len(B))
	for b := range B[0] {
		for i := range B {
			buf[i] = B[i][b]
		}
		for a := range A {
			_acc := archsimd.BroadcastFloat32x16(0.0)
			for i := 0; i < len(A[a]); {
				_as, di := archsimd.LoadFloat32x16Part(A[a][i:])
				_bs, _ := archsimd.LoadFloat32x16Part(buf[i:])
				_acc = _as.MulAdd(_bs, _acc)
				i += di
			}
			_hi8, _lo8 := _acc.GetHi(), _acc.GetLo()
			_hi8 = _hi8.Add(_lo8)
			_hi4, _lo4 := _hi8.GetHi(), _hi8.GetLo()
			_hi4 = _hi4.Add(_lo4)
			C[a][b] = _hi4.GetElem(0) + _hi4.GetElem(1) + _hi4.GetElem(2) + _hi4.GetElem(3)
		}
	}
}

func mm32x16par(A, B, C [][]float32, step int) {
	worker := func(start, end int) {
		bvec := make([]float32, len(B))
		var buf [16]float32
		for b := start; b < end; b++ {
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
	var wg sync.WaitGroup
	for i := 0; i < len(B); i += step {
		wg.Go(func() { worker(i, min(i+step, len(B))) })
	}
	wg.Wait()
}

func BenchmarkMM64x8(b *testing.B) {
	rng := rand.New(rand.NewSource(42))
	A := randMat[float64](N, rng)
	B := randMat[float64](N, rng)
	C := randMat[float64](N, rng)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mm64x8(A, B, C)
	}
}

func BenchmarkMM32x16(b *testing.B) {
	rng := rand.New(rand.NewSource(42))
	A := randMat[float32](N, rng)
	B := randMat[float32](N, rng)
	C := randMat[float32](N, rng)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mm32x16(A, B, C)
	}
}

func BenchmarkReferenceMM(b *testing.B) {
	rng := rand.New(rand.NewSource(42))
	A := randMat[float64](N, rng)
	B := randMat[float64](N, rng)
	C := randMat[float64](N, rng)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		naive64(A, B, C)
	}
}

func BenchmarkGonumMM(b *testing.B) {
	rng := rand.New(rand.NewSource(42))
	Adata := make([]float64, N*N)
	Bdata := make([]float64, N*N)
	Cdata := make([]float64, N*N)
	for i := range Adata {
		Adata[i] = rng.NormFloat64()
		Bdata[i] = rng.NormFloat64()
	}
	A := mat.NewDense(N, N, Adata)
	B := mat.NewDense(N, N, Bdata)
	C := mat.NewDense(N, N, Cdata)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		C.Mul(A, B)
	}
}

func BenchmarkMM32x16par(b *testing.B) {
	rng := rand.New(rand.NewSource(42))
	A := randMat[float32](N, rng)
	B := randMat[float32](N, rng)
	C := randMat[float32](N, rng)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mm32x16par(A, B, C, N/32)
	}
}

func randMat[T float64 | float32](n int, rng *rand.Rand) [][]T {
	m := make([][]T, n)
	for i := range m {
		m[i] = make([]T, n)
	}
	for i := range m {
		for j := range m[i] {
			m[i][j] = T(rng.NormFloat64())
		}
	}
	return m
}

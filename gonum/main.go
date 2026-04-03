package main

import (
	"log"
	"math/rand/v2"

	"gonum.org/v1/gonum/mat"
)

func mulMatT(c, a, b [][]float64) {
	if len(a[0]) != len(b[0]) || len(c[0]) != len(b) || len(c) != len(a) {
		log.Panicf("mulMatT: bad matrix dimensions, A: %dx%d, B: %dx%d, C: %dx%d\n", len(a), len(a[0]), len(b), len(b[0]), len(c), len(c[0]))
	}
	for i := range a {
		for j := range b {
			sum := 0.0
			for k := range b[0] {
				sum += a[i][k] * b[j][k]
			}
			c[i][j] = sum
		}
	}
}

func naive(r, c int) [][]float64 {
	A := make([][]float64, r)
	for i := range A {
		A[i] = make([]float64, c)
		for j := range A[i] {
			A[i][j] = rand.Float64()
		}
	}
	return A
}

func gonum(r, c int) *mat.Dense {
	return mat.NewDense(r, c, gen(r*c))
}

func gen(n int) []float64 {
	vec := make([]float64, n)
	for i := range len(vec) {
		vec[i] = rand.Float64()
	}
	return vec
}

func main() {
	A := gonum(30, 30)
	for range 1000000 {
		A.Mul(A, A)
	}
	// A := naive(30, 30)
	// for range 1000000 {
	// 	mulMatT(A, A, A)
	// }
}

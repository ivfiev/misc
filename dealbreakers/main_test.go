package main

import (
	"fmt"
	"testing"
)

func basicM(x float64) *M {
	return &M{X: []float64{x, x, x}}
}

func basicW(x float64) *W {
	return NewW(
		[]float64{x, x, x},
		[]float64{0.4, 0.3, 0.3},
		[]float64{0.1, 0.5},
		0.1,
		0.25,
		0.1,
	)
}

func Test00(t *testing.T) {
	m := basicM(0)
	w := basicW(0)
	fmt.Printf("00: %f\n", w.Yes(m))
}

func Test01(t *testing.T) {
	m := basicM(0)
	w := basicW(1)
	fmt.Printf("01: %f\n", w.Yes(m))
}

func Test02(t *testing.T) {
	m := basicM(0)
	w := basicW(2)
	fmt.Printf("02: %f\n", w.Yes(m))
}

func Test10(t *testing.T) {
	m := basicM(1)
	w := basicW(0)
	fmt.Printf("10: %f\n", w.Yes(m))
}

func Test20(t *testing.T) {
	m := basicM(2)
	w := basicW(0)
	fmt.Printf("20: %f\n", w.Yes(m))
}

func Test11(t *testing.T) {
	m := basicM(1)
	w := basicW(1)
	fmt.Printf("11: %f\n", w.Yes(m))
}

func Test12(t *testing.T) {
	m := basicM(1)
	w := basicW(2)
	fmt.Printf("12: %f\n", w.Yes(m))
}

func Test22(t *testing.T) {
	m := basicM(2)
	w := basicW(2)
	fmt.Printf("22: %f\n", w.Yes(m))
}

func Test_10(t *testing.T) {
	m := basicM(-1)
	w := basicW(0)
	fmt.Printf("-10: %f\n", w.Yes(m))
}

func Test_1_2(t *testing.T) {
	m := basicM(-1)
	w := basicW(-2)
	fmt.Printf("-1-2: %f\n", w.Yes(m))
}

func Test0_2(t *testing.T) {
	m := basicM(0)
	w := basicW(-2)
	fmt.Printf("0-2: %f\n", w.Yes(m))
}

func Test50(t *testing.T) {
	m := basicM(5)
	w := basicW(0)
	fmt.Printf("50: %f\n", w.Yes(m))
}

func Test55(t *testing.T) {
	m := basicM(5)
	w := basicW(5)
	fmt.Printf("55: %f\n", w.Yes(m))
}

func Test25(t *testing.T) {
	m := basicM(2)
	w := basicW(5)
	fmt.Printf("25: %f\n", w.Yes(m))
}

func TestMixed1(t *testing.T) {
	m := &M{X: []float64{-1, 2, 1}}
	w := NewW(
		[]float64{2, -1, -1},
		[]float64{0.9, 0.1, 0.1},
		[]float64{0.1, 0.5},
		0.1,
		0.25,
		0.1,
	)
	fmt.Printf("mixed1: %f\n", w.Yes(m))
}

func TestMixed2(t *testing.T) {
	m := &M{X: []float64{-1, 2, 1}}
	w := NewW(
		[]float64{2, -1, -1},
		[]float64{0.05, 0.9, 0.05},
		[]float64{0.1, 0.5},
		0.1,
		0.25,
		0.1,
	)
	fmt.Printf("mixed2: %f\n", w.Yes(m))
}

func TestMixed3(t *testing.T) {
	m := &M{X: []float64{0.5, 1.5, -1}}
	w := NewW(
		[]float64{-1, 1.4, 1.5},
		[]float64{0.2, 0.3, 0.5},
		[]float64{0.1, 0.5},
		0.1,
		0.25,
		0.1,
	)
	fmt.Printf("mixed3: %f\n", w.Yes(m))
}

func TestMixed4(t *testing.T) {
	m := &M{X: []float64{3, -1, -1}}
	w := NewW(
		[]float64{3, -1, -1},
		[]float64{0.8, 0.2, 0.0},
		[]float64{0.1, 0.5},
		0.1,
		0.25,
		0.1,
	)
	fmt.Printf("mixed4: %f\n", w.Yes(m))
}

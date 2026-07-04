package main

import (
	"fmt"
	"testing"
)

var (
	Wx = trainWx(NewRandom(42))
	Wy = trainWy(NewRandom(43))
)

func basicMx(x float64) *M {
	return NewM([]float64{x, x, x}, nil)
}

func basicWx(x float64) *W {
	return NewW(
		[]float64{x, x, x},
		[]float64{0.4, 0.3, 0.3},
		Wx,
		nil, nil,
	)
}

func basicMy(y float64) *M {
	return NewM(nil, []float64{y, y})
}

func advMy(y []float64) *M {
	return NewM(nil, y)
}

func basicWy(y float64) *W {
	return NewW(
		nil, nil, nil,
		[]float64{y, y}, Wy,
	)
}

func advWy(y []float64) *W {
	return NewW(
		nil, nil, nil,
		y, Wy,
	)
}

func Test00(t *testing.T) {
	m := basicMx(0)
	w := basicWx(0)
	fmt.Printf("00: %f\n", w.Covfefe(m))
}

func Test01(t *testing.T) {
	m := basicMx(0)
	w := basicWx(1)
	fmt.Printf("01: %f\n", w.Covfefe(m))
}

func Test02(t *testing.T) {
	m := basicMx(0)
	w := basicWx(2)
	fmt.Printf("02: %f\n", w.Covfefe(m))
}

func Test10(t *testing.T) {
	m := basicMx(1)
	w := basicWx(0)
	fmt.Printf("10: %f\n", w.Covfefe(m))
}

func Test20(t *testing.T) {
	m := basicMx(2)
	w := basicWx(0)
	fmt.Printf("20: %f\n", w.Covfefe(m))
}

func Test11(t *testing.T) {
	m := basicMx(1)
	w := basicWx(1)
	fmt.Printf("11: %f\n", w.Covfefe(m))
}

func Test12(t *testing.T) {
	m := basicMx(1)
	w := basicWx(2)
	fmt.Printf("12: %f\n", w.Covfefe(m))
}

func Test22(t *testing.T) {
	m := basicMx(2)
	w := basicWx(2)
	fmt.Printf("22: %f\n", w.Covfefe(m))
}

func Test_10(t *testing.T) {
	m := basicMx(-1)
	w := basicWx(0)
	fmt.Printf("-10: %f\n", w.Covfefe(m))
}

func Test_1_2(t *testing.T) {
	m := basicMx(-1)
	w := basicWx(-2)
	fmt.Printf("-1-2: %f\n", w.Covfefe(m))
}

func Test0_2(t *testing.T) {
	m := basicMx(0)
	w := basicWx(-2)
	fmt.Printf("0-2: %f\n", w.Covfefe(m))
}

func Test40(t *testing.T) {
	m := basicMx(4)
	w := basicWx(0)
	fmt.Printf("40: %f\n", w.Covfefe(m))
}

func Test44(t *testing.T) {
	m := basicMx(4)
	w := basicWx(4)
	fmt.Printf("44: %f\n", w.Covfefe(m))
}

func Test24(t *testing.T) {
	m := basicMx(2)
	w := basicWx(4)
	fmt.Printf("24: %f\n", w.Covfefe(m))
}

func TestY00(t *testing.T) {
	w := basicWy(0)
	m := basicMy(0)
	_, p := w.LTR(m)
	fmt.Printf("Y00: %f\n", p)
}

func TestY11(t *testing.T) {
	w := basicWy(1)
	m := basicMy(1)
	_, p := w.LTR(m)
	fmt.Printf("Y11: %f\n", p)
}

func TestY22(t *testing.T) {
	w := basicWy(2)
	m := basicMy(2)
	_, p := w.LTR(m)
	fmt.Printf("Y22: %f\n", p)
}

func TestY_1_1(t *testing.T) {
	w := basicWy(-1)
	m := basicMy(-1)
	_, p := w.LTR(m)
	fmt.Printf("Y-1-1: %f\n", p)
}

func TestY_2_2(t *testing.T) {
	w := basicWy(-2)
	m := basicMy(-2)
	_, p := w.LTR(m)
	fmt.Printf("Y-2-2: %f\n", p)
}

func TestY_pol0(t *testing.T) {
	w := advWy([]float64{-2, 0})
	m := advMy([]float64{2, 0})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol0: %f\n", p)
}

func TestY_pol1(t *testing.T) {
	w := advWy([]float64{-2, 1})
	m := advMy([]float64{2, 1})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol1: %f\n", p)
}

func TestY_pol2(t *testing.T) {
	w := advWy([]float64{-2, 2})
	m := advMy([]float64{2, 2})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol2: %f\n", p)
}

func TestY_pol_2(t *testing.T) {
	w := advWy([]float64{-2, -2})
	m := advMy([]float64{2, -2})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol-2: %f\n", p)
}

func TestY_pol_22(t *testing.T) {
	w := advWy([]float64{2, 0})
	m := advMy([]float64{-2, 0})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol-22: %f\n", p)
}

func TestY_pol_222(t *testing.T) {
	w := advWy([]float64{-2, 0})
	m := advMy([]float64{2, 0})
	_, p := w.LTR(m)
	fmt.Printf("Y-pol-22: %f\n", p)
}

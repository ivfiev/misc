package main

import (
	"fmt"
	"math"
)

func addmul(xs ...any) []float64 {
	res := make([]float64, len(xs[0].([]float64)))
	for i := range res {
		for j := 0; j < len(xs); j += 2 {
			res[i] += xs[j].([]float64)[i] * xs[j+1].(float64)
		}
	}
	return res
}

func solveIVP(
	ys0 []float64,
	f func(t float64, ys []float64) []float64,
	t0 float64,
) func(t float64) []float64 {
	h := 0.001
	return func(tn float64) []float64 {
		n := int((tn - t0) / h)
		t := t0
		ys := append([]float64(nil), ys0...)
		for range n {
			k1 := f(t, ys)
			k2 := f(t+h/2, addmul(ys, 1.0, k1, h/2))
			k3 := f(t+h/2, addmul(ys, 1.0, k2, h/2))
			k4 := f(t+h, addmul(ys, 1.0, k3, h))
			ys = addmul(ys, 1.0, addmul(k1, 1.0, k2, 2.0, k3, 2.0, k4, 1.0), h/6)
			t += h
		}
		return ys
	}
}

func test() {
	fmt.Printf("Expected 0.000713, actual %v\n",
		solveIVP([]float64{0},
			func(t float64, ys []float64) []float64 {
				return []float64{math.Cos(1000*t) + math.Sin(1000*t)}
			}, -123)(123))
}

func main() {
	// test()
	// ivp := solveIVP([]float64{25000, 0, 0, 0.105, 0.19, 0.10}, salarySavings, 0)
	// fmt.Printf("%v\n", ivp(10000))
	// ivp := solveIVP([]float64{0.99, 0.01, 1}, automation, 0)
	// for t := range 11 {
	// 	fmt.Printf("%v\n", ivp(float64(t)))
	// }
	ivp := solveIVP([]float64{100, 50}, minsky, 0)
	for t := range 20 {
		fmt.Printf("%v\n", ivp(float64(t)))
	}
}

func salarySavings(t float64, y []float64) []float64 {
	const (
		salary = iota
		savings
		stonks
		salaryRate
		savingsRate
		stonksRate
	)
	return []float64{
		y[salary] * y[salaryRate],
		y[salary] * y[savingsRate],
		y[salary]*y[savingsRate] + y[savings]*y[stonksRate],
		0,
		0,
		0,
	}
}

func automation(t float64, y []float64) []float64 {
	const (
		humans = iota
		robots
	)
	const k = 0.05
	return []float64{
		-k * y[robots] * y[humans],
		k * y[robots] * (1 - y[robots]),
	}
}

func minsky(t float64, y []float64) []float64 {
	const (
		Y = iota
		D
	)
	const (
		g   = 0.05
		r   = 0.02
		phi = 0.02
		th  = 0.01
	)
	return []float64{
		g*y[Y] - r*y[D],
		r*y[D] + phi*y[Y] - th*y[D],
	}
}

// TODO model learning/burnout logistic curves

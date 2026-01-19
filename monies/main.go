package main

import (
	"errors"
	"fmt"
	"log"
	"math"
)

const EPS = 0.000001

type JobReq struct {
	kind string
	id   int
	f    func(float64) float64
	a    float64
	b    float64
}

type JobRes struct {
	kind string
	id   int
	a    float64
}

var (
	jobReq chan JobReq = make(chan JobReq)
	jobRes chan JobRes = make(chan JobRes)
)

func worker() {
	for job := range jobReq {
		switch job.kind {
		case "integral":
			sum, _, err := converge(simpson(job.f, job.a, job.b), 128)
			if err != nil {
				log.Fatal(err)
			}
			jobRes <- JobRes{job.kind, job.id, sum}
		}
	}
}

func simpson(f func(float64) float64, a, b float64) func(int) float64 {
	return func(ds int) float64 {
		sum := 0.0
		dx := (b - a) / float64(ds)
		panels := ds / 2
		for p := range panels {
			x := a + float64(p)*2*dx
			y0 := f(x)
			y1 := f(x + dx)
			y2 := f(x + 2*dx)
			sum += dx * (y0 + 4*y1 + y2) / 3.0
		}
		return sum
	}
}

func converge(f func(int) float64, ds int) (float64, int, error) {
	y0 := f(ds)
	for range 22 {
		ds *= 2
		y1 := f(ds)
		if math.Abs(y0-y1) < EPS {
			return y1, ds, nil
		}
		y0 = y1
	}
	return 0, 0, errors.New("function did not converge")
}

func integrate(f func(float64) float64, a, b float64) float64 {
	const jobs = 10
	h := (b - a) / jobs
	for j := range jobs {
		jobReq <- JobReq{
			kind: "integral", id: j,
			f: f,
			a: a + (float64(j) * h),
			b: a + (float64(1+j) * h),
		}
	}
	sum := 0.0
	for range jobs {
		res := <-jobRes
		sum += res.a
	}
	return sum
}

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
	fmt.Printf("Expected 0.000713, actual %f\n",
		integrate(func(x float64) float64 {
			return math.Cos(1000*x) + math.Sin(1000*x)
		}, -123, 123))
	fmt.Printf("Expected 80305.58, actual %f\n",
		integrate(func(x float64) float64 {
			return math.Exp(x * 0.1337)
		}, -13, 69.420))
	fmt.Printf("Expected 0.000713, actual %v\n",
		solveIVP([]float64{0},
			func(t float64, ys []float64) []float64 {
				return []float64{math.Cos(1000*t) + math.Sin(1000*t)}
			}, -123)(123))
}

func main() {
	for range 16 {
		go worker()
	}
	test()
	ivp := solveIVP([]float64{25000, 0, 0, 0.105, 0.19, 0.10}, dxdys, 0)
	fmt.Printf("%v\n", ivp(10))
}

func dxdys(t float64, y []float64) []float64 {
	const (
		salary = iota
		savings
		stonks
		salaryRate
		savingsRate
		stonkRate
	)
	return []float64{
		y[salary] * y[salaryRate],
		y[salary] * y[savingsRate],
		y[salary]*y[savingsRate] + y[savings]*y[stonkRate],
		0,
		0,
		0,
	}
}

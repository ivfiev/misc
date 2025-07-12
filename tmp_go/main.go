package main

import (
	"fmt"
	"log"
)

func avx(a, b []float32) ([]float32, error) {
	if len(a) != len(b) {
		return nil, fmt.Errorf("slices wrong lengths")
	}
	c := make([]float32, len(a))
	for i := range a {
		c[i] = a[i] * b[i]
	}
	return c, nil
}

func main() {
	a := []float32{1, 2, 3, 5, 6}
	b := []float32{10, 20, 30, 40, 60}
	c, err := avx(a, b)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(c)
}

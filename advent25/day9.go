package main

import (
	"fmt"
	"simd/archsimd"
	"strconv"
	"strings"
)

func parse9() ([]int64, []int64) {
	lines := readFile(9)
	xs, ys := []int64{}, []int64{}
	for _, line := range lines {
		words := strings.Split(line, ",")
		x, _ := strconv.Atoi(words[0])
		y, _ := strconv.Atoi(words[1])
		xs = append(xs, int64(x))
		ys = append(ys, int64(y))
	}
	return xs, ys
}

func day9() {
	xs, ys := parse9()
	fmt.Printf("%v %v\n", xs, ys)
	_maxes := archsimd.BroadcastInt64x8(0)
	_ones := archsimd.BroadcastInt64x8(1)
	for i := range xs {
		_x0 := archsimd.BroadcastInt64x8(xs[i])
		_y0 := archsimd.BroadcastInt64x8(ys[i])
		for j := i + 1; j < len(xs); {
			_xs1, dj := archsimd.LoadInt64x8Part(xs[j:])
			_ys1, _ := archsimd.LoadInt64x8Part(ys[j:])
			_mask := archsimd.Mask64x8FromBits((1 << dj) - 1)
			_top := _x0.Sub(_xs1).Abs().Add(_ones)
			_side := _y0.Sub(_ys1).Abs().Add(_ones)
			_areas := _top.Mul(_side).Masked(_mask)
			_maxes = _maxes.Max(_areas)
			j += dj
		}
	}
	_4h, _4l := _maxes.GetHi(), _maxes.GetLo()
	_4h = _4h.Max(_4l)
	_2h, _2l := _4h.GetHi(), _4h.GetLo()
	_2h = _2h.Max(_2l)
	maxArea := max(_2h.GetElem(0), _2h.GetElem(1))
	println(maxArea)
}

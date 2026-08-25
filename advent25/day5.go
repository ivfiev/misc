package main

import (
	"math/bits"
	"simd/archsimd"
	"strconv"
	"strings"
)

func parse5() ([]int64, []int64, []int64) {
	lines := readFile(5)
	from, until, ids := []int64{}, []int64{}, []int64{}
	for _, line := range lines {
		if line == "" {
			continue
		}
		words := strings.Split(line, "-")
		if len(words) == 2 {
			f, _ := strconv.Atoi(words[0])
			t, _ := strconv.Atoi(words[1])
			from = append(from, int64(f))
			until = append(until, int64(t))
		}
		if len(words) == 1 {
			id, _ := strconv.Atoi(words[0])
			ids = append(ids, int64(id))
		}
	}
	return from, until, ids
}

func day5() {
	count := 0
	from, until, ids := parse5()
	for i := 0; i < len(ids); {
		_ids, di := archsimd.LoadInt64x8Part(ids[i:])
		_mask := archsimd.Mask64x8FromBits(0b00000000)
		for j := range from {
			_from := archsimd.BroadcastInt64x8(from[j])
			_until := archsimd.BroadcastInt64x8(until[j])
			_gt := _ids.GreaterEqual(_from)
			_lt := _ids.LessEqual(_until)
			_mask = _mask.Or(_gt.And(_lt))
		}
		count += bits.OnesCount(uint(_mask.ToBits()))
		i += di
	}
	println(count)
}

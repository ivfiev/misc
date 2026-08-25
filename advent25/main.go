package main

import (
	"log"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		panic("invalid args, expecting 'day##'")
	}
	day := os.Args[1]
	switch day {
	case "day5":
		day5()
	case "day9":
		day9()
	default:
		log.Panicf("Day '%s' not solved, try another 'day##'", day)
	}
}

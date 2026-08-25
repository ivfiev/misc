package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

func readFile(day int) []string {
	filename := fmt.Sprintf("day%d.txt", day)
	fd, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer fd.Close()
	buf := make([]byte, 4096)
	sb := strings.Builder{}
	for {
		n, err := fd.Read(buf)
		if err != nil {
			if err == io.EOF {
				return strings.Split(strings.TrimSpace(sb.String()), "\n")
			}
			panic(err)
		}
		if n > 0 {
			_, err = sb.Write(buf[:n])
			if err != nil {
				panic(err)
			}
		}
	}
}

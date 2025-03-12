package main

import "fmt"

func check() {
	fmt.Println("declared and not used")
}

func main() {
	fmt.Println("entering B")
	check()
	fmt.Println("exiting B")
}

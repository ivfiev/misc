package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
)

func main() {
	fmt.Println("entering A")
	cmd := exec.Command("./bin/B")
	cmd.Stdout = os.Stdout
	err := cmd.Run()
	if err != nil {
		log.Fatalf("Failed to start B", err)
	}
	fmt.Println("exiting A")
}

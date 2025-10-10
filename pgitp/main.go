package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
)

type args struct {
	mode    string
	pass    string
	exclude []string
}

func main() {
	args := parseArgs()
	switch args.mode {
	case "in":
		in(args)
	case "out":
		out(args)
	}
}

func parseArgs() args {
	if len(os.Args) < 2 {
		log.Fatalf("Usage: baaee in/out -p password -e exclude1 -e exclude2")
	}
	args := args{"", "P@ssword1", []string{}}
	if os.Args[1] != "in" && os.Args[1] != "out" {
		log.Fatalf("Invalid mode selected: %s", os.Args[1])
	}
	args.mode = os.Args[1]
	for i := 2; i < len(os.Args)-1; i++ {
		if os.Args[i] == "-p" {
			args.pass = os.Args[i+1]
			i++
		}
		if os.Args[i] == "-e" {
			args.exclude = append(args.exclude, os.Args[i+1])
			i++
		}
	}
	return args
}

func in(args args) {
	tarArgs := []string{"-czf", "-"}
	for _, e := range args.exclude {
		tarArgs = append(tarArgs, fmt.Sprintf("--exclude=%s", e))
	}
	tarArgs = append(tarArgs, ".")
	tar := exec.Command("tar", tarArgs...)
	tar.Stderr = os.Stderr
	tarStdout, err := tar.StdoutPipe()
	if err != nil {
		log.Fatalf("pipe failed: %v", err)
	}
	gpg := exec.Command("gpg", "--symmetric", "--cipher-algo", "AES256", "--armor", "--batch", "--yes", "--passphrase", args.pass)
	gpg.Stdin = tarStdout
	gpg.Stdout = os.Stdout
	gpg.Stderr = os.Stderr
	if err = tar.Start(); err != nil {
		log.Fatalf("tar failed: %v", err)
	}
	if err = gpg.Start(); err != nil {
		log.Fatalf("base64 failed: %v", err)
	}
	if err = gpg.Wait(); err != nil {
		log.Fatalf("base64 failed: %v", err)
	}
	if err = tar.Wait(); err != nil {
		log.Fatalf("tar failed: %v", err)
	}
}

func out(args args) {
	gpg := exec.Command("gpg", "--decrypt", "--passphrase", args.pass, "--batch")
	gpg.Stdin = os.Stdin
	// gpg.Stderr = os.Stderr
	gpgStdout, err := gpg.StdoutPipe()
	if err != nil {
		log.Fatalf("pipe failed: %v", err)
	}
	tar := exec.Command("tar", "-xzf", "-")
	tar.Stdin = gpgStdout
	tar.Stdout = os.Stdout
	tar.Stderr = os.Stderr
	if err = gpg.Start(); err != nil {
		log.Fatalf("base64 failed: %v", err)
	}
	if err = tar.Start(); err != nil {
		log.Fatalf("tar failed: %v", err)
	}
	if err = tar.Wait(); err != nil {
		log.Fatalf("tar failed: %v", err)
	}
	if err = gpg.Wait(); err != nil {
		log.Fatalf("base64 failed: %v", err)
	}
}

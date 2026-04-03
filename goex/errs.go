package main

import (
	"errors"
	"fmt"
)

type Unexpected struct {
	msg string
	err error
}

func (u *Unexpected) Error() string {
	return u.msg
}

func (u *Unexpected) Unwrap() error {
	return u.err
}

var errSentinel = errors.New("oops")

func w() error {
	return fmt.Errorf("sentinel err %w", errSentinel)
}

func v() error {
	return fmt.Errorf("sentinel err %v", errSentinel)
}

func u(err error) error {
	if err != nil {
		return &Unexpected{"unexpected err", err}
	} else {
		return nil
	}
}

func u2() error {
	return nil
}

func main() {
	println(errors.Is(w(), errSentinel))
	println(errors.Is(v(), errSentinel))
	if unexpected, ok := errors.AsType[*Unexpected](u(errors.New("oops2"))); ok {
		println(unexpected.msg)
	}
	println(errors.Is(u(nil), errSentinel))
	println(errors.Is(u(errors.New("oops4")), errSentinel))
	println(errors.Is(u(errSentinel), errSentinel))
}

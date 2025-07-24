package main

import (
	"fmt"
	"log"
	"math"
	"math/big"
	"math/rand"
	"time"
)

var rnd = rand.New(rand.NewSource(time.Now().UnixMilli()))
var one = big.NewInt(1)
var two = big.NewInt(2)

func isEven(n *big.Int) bool {
	return new(big.Int).Mod(n, big.NewInt(2)).Int64() == 0
}

func millerTest(n *big.Int, prob float64) bool {
	if n.Cmp(two) == 0 {
		return true
	}
	if n.Cmp(two) < 0 || isEven(n) {
		return false
	}
	a := new(big.Int)
	x := new(big.Int)
	y := new(big.Int)
	none := new(big.Int).Sub(n, one)
	k := int(math.Log(1-prob)/math.Log(0.25) + 1)
	for range k {
		a.Rand(rnd, new(big.Int).Sub(n, big.NewInt(3))).Add(a, two)
		pow := new(big.Int).Set(none)
		x.Set(a).Exp(a, pow, n)
		if x.Cmp(one) != 0 {
			return false
		}
		for isEven(pow) {
			pow.Div(pow, two)
			y.Set(a).Exp(a, pow, n)
			if y.Cmp(one) != 0 {
				if y.Cmp(none) != 0 {
					return false
				}
				break
			}
		}
	}
	return true
}

func randBig(bits int) *big.Int {
	nibbles := make([]byte, 0, bits/4)
	hex := "0123456789abcdef"
	nibbles = append(nibbles, hex[1:][rnd.Int31()%15])
	for range cap(nibbles) - 1 {
		nibbles = append(nibbles, hex[rnd.Int31()%16])
	}
	num, ok := new(big.Int).SetString(string(nibbles), 16)
	if !ok {
		log.Fatalf("randBig")
	}
	return num
}

func randBigPrime(bits int, prob float64) *big.Int {
	num := randBig(bits)
	for !millerTest(num, prob) {
		num.Add(num, one)
	}
	return num
}

func pq() (*big.Int, *big.Int) {
	for {
		p := randBigPrime(256, 0.9999999)
		q := new(big.Int).Set(p)
		q.Sub(q, one).Div(q, two)
		if millerTest(q, 0.9999999) {
			return p, q
		}
	}
}

func pg() (*big.Int, *big.Int) {
	p, q := pq()
	for {
		g := new(big.Int).Rand(rnd, p)
		if new(big.Int).Exp(g, q, p).Cmp(one) != 0 {
			return p, g
		}
	}
}

func main() {
	verify()
	p, g := pg()
	fmt.Printf("p: %d\ng: %d\n\n", p, g)
	alice := new(big.Int).Rand(rnd, p)
	bob := new(big.Int).Rand(rnd, p)
	fmt.Printf("private key a: %d\nprivate key b: %d\n\n", alice, bob)
	a := new(big.Int).Exp(g, alice, p)
	b := new(big.Int).Exp(g, bob, p)
	fmt.Printf("public key a: %d\npublic key b: %d\n\n", a, b)
	a.Exp(a, bob, p)
	b.Exp(b, alice, p)
	fmt.Printf("symmetric key a: %d\nsymmetric key b: %d\n\n", a, b)
}

func naiveTest(n int) bool {
	if n == 2 {
		return true
	}
	if n < 2 || n%2 == 0 {
		return false
	}
	for i := 3; i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func verify() {
	for i := range 10000 {
		miller := millerTest(big.NewInt(int64(i)), 0.99999)
		naive := naiveTest(i)
		if miller && !naive || naive && !miller {
			log.Fatalf("Error in prime test")
		}
	}
}

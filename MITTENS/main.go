package main

import (
	"fmt"
	"math/rand"
	"slices"
)

const (
	N = 2000
	S = 500
)

type Genome struct {
	code    []int
	fitness int
}

func randomGenome() Genome {
	genome := make([]int, 0, N)
	for range N {
		bit := rand.Int() & 1
		genome = append(genome, bit)
	}
	return Genome{genome, -1}
}

func fitness(target, genome Genome) int {
	sum := 0
	for i := range len(genome.code) {
		if target.code[i] == genome.code[i] {
			sum++
		}
	}
	return sum
}

func mutate(genome *Genome, prob float32) {
	for i := range len(genome.code) {
		if rand.Float32() < prob {
			if genome.code[i] == 1 {
				genome.code[i] = 0
			} else {
				genome.code[i] = 1
			}
		}
	}
	genome.fitness = -1
}

func printStats(target Genome, population []Genome) {
	maxFitness := 0
	avgFitness := float32(0)
	almost := 0
	for _, genome := range population {
		maxFitness = max(maxFitness, genome.fitness)
		avgFitness += float32(genome.fitness)
	}
	avgFitness /= S
	for i := range N {
		count := 0
		for _, genome := range population {
			if genome.code[i] == target.code[i] {
				count++
			}
		}
		if count > 0.9*S {
			almost++
		}
	}
	fmt.Printf("Fittest: %d, Average: %.2f, Almost: %d\n", maxFitness, avgFitness, almost)
}

func selection() {
	target := randomGenome()
	population := make([]Genome, 0, S)
	for range S {
		population = append(population, randomGenome())
	}
	for {
		for i := range population {
			population[i].fitness = fitness(target, population[i])
		}
		printStats(target, population)
		slices.SortFunc(population,
			func(g1, g2 Genome) int {
				return g2.fitness - g1.fitness
			})
		survivors := population[:S/4]
		newPopulation := make([]Genome, 0, S)
		for _, survivor := range survivors {
			for range 4 {
				child := Genome{make([]int, N), -1}
				copy(child.code, survivor.code)
				mutate(&child, 1.0/N)
				newPopulation = append(newPopulation, child)
			}
		}
		population = newPopulation
	}
}

func main() {
	selection()
}

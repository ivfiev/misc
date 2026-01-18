package main

import (
	"fmt"
	"math/rand"
	"slices"
	"sync"
	"time"
)

// 34.5 secs on r7 350
const (
	N = 7000
	S = 700
	F = 4
)

type Genome struct {
	code    []byte
	fitness int // cached fitness
}

func randomGenome(rng *rand.Rand) *Genome {
	genome := make([]byte, 0, N)
	for range N {
		bit := byte(rng.Int31() & 1)
		genome = append(genome, bit)
	}
	return &Genome{genome, -1}
}

func (g *Genome) copy() *Genome {
	child := Genome{make([]byte, N), -1}
	copy(child.code, g.code)
	return &child
}

func (g *Genome) calcFitness(target *Genome) {
	sum := 0
	for i := range len(g.code) {
		if target.code[i] == g.code[i] {
			sum++
		}
	}
	g.fitness = sum
}

func (g *Genome) mutateCode(rng *rand.Rand, prob float32) {
	for i := range len(g.code) {
		if rng.Float32() < prob {
			g.code[i] ^= 1
		}
	}
	g.fitness = -1
}

func printStats(generation int, jobs chan Job, target *Genome, population []*Genome) {
	var counts [N]int
	maxFitness := 0
	avgFitness := float32(0)
	p00, p25, p50, p75, p90, p98 := 0, 0, 0, 0, 0, 0
	for _, genome := range population {
		maxFitness = max(maxFitness, genome.fitness)
		avgFitness += float32(genome.fitness)
	}
	avgFitness /= S
	var wg sync.WaitGroup
	for i := range N {
		wg.Add(1)
		jobs <- Job{
			jobType:    1,
			genome:     target,
			population: &population,
			counts:     &counts,
			index:      i,
			wg:         &wg,
		}
	}
	wg.Wait()
	for i := range N {
		count := counts[i]
		if count > 0.98*S {
			p98++
		} else if count > 0.9*S {
			p90++
		} else if count > 0.75*S {
			p75++
		} else if count > 0.5*S {
			p50++
		} else if count > 0.25*S {
			p25++
		} else {
			p00++
		}
	}
	fmt.Printf("Generation: %d\n", generation)
	fmt.Printf("Fittest: %d, Average: %.2f\n", maxFitness, avgFitness)
	fmt.Printf(
		"p98: %.3f (%d)\np90: %.3f (%d)\np75: %.3f (%d)\np50: %.3f (%d)\np25: %.3f (%d)\np00: %.3f (%d)\n\n",
		float32(p98)/N, p98,
		float32(p90)/N, p90,
		float32(p75)/N, p75,
		float32(p50)/N, p50,
		float32(p25)/N, p25,
		float32(p00)/N, p00)
}

func selection(jobs chan Job) {
	var wg sync.WaitGroup
	var mut sync.Mutex
	rng := rand.New(rand.NewSource(time.Now().UnixNano()))
	target := randomGenome(rng)
	population := make([]*Genome, 0, S)
	for range S {
		population = append(population, randomGenome(rng))
	}
	for generation := 1; ; generation++ {
		for i := range S {
			population[i].calcFitness(target)
		}
		if generation%10 == 0 {
			printStats(generation, jobs, target, population)
		}
		slices.SortFunc(population,
			func(g1, g2 *Genome) int {
				return g2.fitness - g1.fitness
			})
		survivors := population[:S/F]
		newPopulation := make([]*Genome, 0, S)
		for _, survivor := range survivors {
			wg.Add(1)
			jobs <- Job{
				jobType:    0,
				genome:     survivor,
				population: &newPopulation,
				mutex:      &mut,
				wg:         &wg,
			}
		}
		wg.Wait()
		population = newPopulation
	}
}

func main() {
	jobs := make(chan Job)
	for i := range 64 {
		go worker(i, jobs)
	}
	selection(jobs)
	close(jobs)
}

type Job struct {
	jobType    int
	genome     *Genome
	population *[]*Genome
	counts     *[N]int
	index      int
	mutex      *sync.Mutex
	wg         *sync.WaitGroup
}

func worker(id int, jobs <-chan Job) {
	rng := rand.New(rand.NewSource(time.Now().UnixNano() + int64(id*1_000_000)))
	for job := range jobs {
		switch job.jobType {
		case 0:
			for range F {
				child := job.genome.copy()
				child.mutateCode(rng, 1.0/N)
				job.mutex.Lock()
				*job.population = append(*job.population, child)
				job.mutex.Unlock()
			}
			job.wg.Done()
		case 1:
			i := job.index
			counts := job.counts
			target := job.genome
			for _, genome := range *job.population {
				if genome.code[i] == target.code[i] {
					counts[i]++
				}
			}
			job.wg.Done()
		}
	}
}

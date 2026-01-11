import random
import time


N = 2000
S = 500


def random_genome():
    genome = []
    for i in range(N):
        genome.append(0 if random.random() < 0.5 else 1)
    return genome


def fitness(target, genome):
    assert len(target) == len(genome)
    sum = 0
    for i in range(len(genome)):
        sum += int(target[i] == genome[i])
    return sum


def mutate(prob, genome):
    for i in range(len(genome)):
        if random.random() < prob:
            genome[i] = 0 if genome[i] == 1 else 1


def print_stats(target, population):
    fittest = max(fitness(target, genome) for genome in population)
    average = sum(fitness(target, genome) for genome in population) / S
    fixed, segregating, extinct = 0, 0, 0
    almost = 0
    for i in range(N):
        count = sum(int(genome[i] == target[i]) for genome in population)
        fixed += int(count == S)
        segregating += int(0 < count < S)
        extinct += int(count == 0)
        almost += int(count > S * 0.9)
    print(f"Fittest: {fittest}", end=", ")
    print(f"Average: {average}", end="\n")
    print(f"Fixed: {fixed}", end=", ")
    print(f"Segregating: {segregating}", end=", ")
    print(f"Extinct: {extinct}", end=", ")
    print(f"Almost: {almost}")


def selection():
    target = random_genome()
    population = [random_genome() for _ in range(S)]
    while True:
        assert len(population) == S
        print_stats(target, population)
        fitnesses = [(fitness(target, population[i]), i) for i in range(S)]
        sorted_fitnesses = sorted(fitnesses, reverse=True)
        sorted_population = [population[i] for (_, i) in sorted_fitnesses]
        survivors = sorted_population[: S // 4]
        new_generation = []
        for genome in survivors:
            for _ in range(4):
                child = genome.copy()
                mutate(1 / N, child)
                new_generation.append(child)
        population = new_generation


if __name__ == "__main__":
    selection()

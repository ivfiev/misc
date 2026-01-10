import random


N = 1000


def random_bit():
    return 0 if random.random() < 0.5 else 1


def random_genome():
    genome = []
    for i in range(N):
        genome.append(random_bit())
    return genome


def fitness(target, genome):
    assert len(target) == len(genome)
    sum = 0
    for i in range(len(genome)):
        sum += 1 if target[i] == genome[i] else 0
    return sum


def mutate(prob, genome):
    for i in range(len(genome)):
        if random.random() < prob:
            genome[i] = 0 if genome[i] == 1 else 1


def selection():
    size = 100
    target = random_genome()
    population = [random_genome() for _ in range(size)]
    while True:
        print(max(fitness(target, genome) for genome in population))
        population = sorted(population, key=lambda g: fitness(target, g), reverse=True)[:size]
        survivors = population[: size // 4]
        new_generation = []
        for genome in survivors:
            for _ in range(4):
                child = genome.copy()
                mutate(0.001, child)
                new_generation.append(child)
        population = new_generation


if __name__ == "__main__":
    selection()

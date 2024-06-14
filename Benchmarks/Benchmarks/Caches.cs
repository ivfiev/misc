using BenchmarkDotNet.Attributes;

namespace Benchmarks;

internal struct S()
{
    public long X = Random.Shared.NextInt64();
    public long Y = Random.Shared.NextInt64();
    public long Z = Random.Shared.NextInt64();
}

internal class C
{
    public long X = Random.Shared.NextInt64();
    public long Y = Random.Shared.NextInt64();
    public long Z = Random.Shared.NextInt64();
}

[MemoryDiagnoser]
public class Caches
{
    private C[] _classes;
    private C[] _shuffledClasses;
    private C[] _shuffledClassesPages;
    private S[] _shuffledStructs;
    private S[] _structs;

    [GlobalSetup]
    public void Setup()
    {
        _structs = new S[1000];

        _shuffledStructs = new S[1000];
        _shuffledStructs.Shuffle();

        _classes = new C[1000];
        _shuffledClasses = new C[1000];
        _shuffledClassesPages = new C[1000000];

        for (int i = 0; i < 1000; i++)
        {
            _classes[i] = new C();
            _shuffledClasses[i] = new C();
        }

        _shuffledClasses.Shuffle();

        for (int i = 0; i < 1000000; i++)
        {
            _shuffledClassesPages[i] = new C();
        }

        _shuffledClassesPages.Shuffle();
    }

    [Benchmark]
    public long Default()
    {
        long sum = 0;

        for (var j = 0; j < 1000; j++)
        for (var i = 0; i < _structs.Length; i++)
        {
            sum += _structs[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long Shuffled()
    {
        long sum = 0;

        for (var j = 0; j < 1000; j++)
        for (var i = 0; i < _shuffledStructs.Length; i++)
        {
            sum += _shuffledStructs[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long Default2()
    {
        long sum = 0;

        for (var j = 0; j < 1000; j++)
        for (var i = 0; i < _classes.Length; i++)
        {
            sum += _classes[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long Shuffled2()
    {
        long sum = 0;

        for (var j = 0; j < 1000; j++)
        for (var i = 0; i < _shuffledClasses.Length; i++)
        {
            sum += _shuffledClasses[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long ShuffledLarge()
    {
        long sum = 0;

        for (var i = 0; i < _shuffledClassesPages.Length; i++)
        {
            sum += _shuffledClassesPages[i].X;
        }

        return sum;
    }
}
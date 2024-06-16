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
    private const int Size = 30_000_000;

    private C[] _classes;
    private C[] _shuffledClasses;
    private S[] _shuffledStructs;
    private S[] _structs;

    [Params(10_000)]
    public int Accesses;

    [GlobalSetup]
    public void Setup()
    {
        _structs = new S[Accesses * 2];
        _shuffledStructs = new S[Accesses * 2];

        _classes = new C[Size];
        _shuffledClasses = new C[Size];

        for (int i = 0; i < Size; i++)
        {
            _classes[i] = new C();
            _shuffledClasses[i] = new C();
        }

        _shuffledClasses.Shuffle();
        _shuffledStructs.Shuffle();
    }

    [Benchmark]
    public long Structs()
    {
        long sum = 0;

        for (var i = 0; i < Accesses; i++)
        {
            sum += _structs[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long StructsShuffled()
    {
        long sum = 0;

        for (var i = 0; i < Accesses; i++)
        {
            sum += _shuffledStructs[i].X;
        }

        return sum;
    }

    [Benchmark]
    public long Classes()
    {
        long sum = 0;

        for (var i = 0; i < Accesses; i++)
        {
            sum += _classes[i].X;
        }

        return sum;
    }


    [Benchmark]
    [Arguments(0.064)]
    [Arguments(16)]
    [Arguments(64)]
    [Arguments(125)]
    [Arguments(1000)]
    public long ClassesShuffled(double mb)
    {
        const long bytes = 40;
        double range = 1024 * 1024 * mb;
        long a = (long)Math.Round(range / bytes);
        double iters = (double)Accesses / a;

        long sum = 0;

        while (iters > 0)
        {
            var lim = (long)(iters >= 1 ? a : iters * a);
            iters--;

            for (var i = 0; i < lim; i++)
            {
                sum += _shuffledClasses[i].X;
            }
        }

        return sum;
    }
}
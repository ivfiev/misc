namespace Benchmarks;

public static class Extensions
{
    public static void Shuffle<T>(this T[] arr)
    {
        for (var i = 0; i < arr.Length; i++)
        {
            var randIx = Random.Shared.Next(i, arr.Length);
            (arr[i], arr[randIx]) = (arr[randIx], arr[i]);
        }
    }
}
using System;
using System.Linq;
using System.Reactive;
using System.Reactive.Linq;

namespace Reactive
{
    static class Extensions
    {
        public static Func<T, T> Identity<T>() => x => x;
        public static Func<T, V> Compose<T, U, V>(this Func<U, V> g, Func<T, U> f) => x => g(f(x));
    }

    class A
    {
        public int X {get;}
        public A(int x) => X = x;
    }

    class Program
    {
        static void Main(string[] args)
        {
            Observable.Range(0, 100)
                .Select(x => new Func<A, A>((A a) => new A(a.X + x)))
                .Aggregate(Extensions.Identity<A>(), Extensions.Compose)
                .Select(f => f(new A(0)))
                .Subscribe(a => Console.WriteLine(a.X));
        }
    }
}

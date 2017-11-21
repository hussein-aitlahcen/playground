using System;
using System.Linq;
using System.Reactive;
using System.Reactive.Linq;
using System.Collections.Immutable;

namespace Reactive
{
    // Syntaxic sugar
    static class Extensions
    {
        public static Func<T, T> Identity<T>() => x => x;
        public static Func<T, V> Compose<T, U, V>(this Func<U, V> g, Func<T, U> f) => x => g(f(x));
        public static IObservable<T> MonoidalAggregation<T>(this IObservable<Func<T, T>> morphismStream, T initial) =>
            morphismStream.Aggregate(Identity<T>(), Compose).Select(f => f(initial));
    }

    enum EventType
    {
        HolyCow,
        FairyDrake,
        Unicorn
    }

    class Event
    {
        public EventType Type { get; }
        public Event(EventType type) => Type = type;
    }

    // Fully immutable
    class A
    {
        public int Cow { get; }
        public ImmutableList<String> Drake { get; }
        public String Unicorn { get; }
        public A() : this(0, ImmutableList<String>.Empty, String.Empty) {}
        public A(int cow, ImmutableList<String> drake, String unicorn)
        {
            Cow = cow;
            Drake = drake;
            Unicorn = unicorn;
        }
        public override String ToString() => $"cow={Cow}, drake={Drake.Count}, unicorn={Unicorn}";
    }

    class Program
    {
        // Imagine a flow comming from different sources an being merged
        static IObservable<Event> EventStream() => new []
        {
            new Event(EventType.Unicorn),
            new Event(EventType.FairyDrake),
            new Event(EventType.Unicorn),
            new Event(EventType.HolyCow),
            new Event(EventType.FairyDrake)
        }.ToObservable();

        // Given an event category, map each event to a morphism in our single object category
        static IObservable<Func<A, A>> MorphismStream(IObservable<Event> eventStream) =>
            // Pretty sad but the compiler cannot infer high order functions...
            eventStream.Select<Event, Func<A, A>>(ev =>
                    {
                        // No pattern matching yet...
                        switch(ev.Type)
                        {
                            case EventType.HolyCow: return a => new A(a.Cow + 1, a.Drake, a.Unicorn);
                            case EventType.FairyDrake: return a => new A(a.Cow, a.Drake.Add("drake"), a.Unicorn);
                            case EventType.Unicorn: return a => new A(a.Cow, a.Drake, a.Unicorn + " UNNNNNNICORN");
                            default: return a => a; // Identity
                        }
                    });

        static void Main(string[] args)
        {
            // Given a single object category A and morphisms A -> A, the monoid operation is the composition.
            MorphismStream(EventStream())
                .MonoidalAggregation(new A())
                .Subscribe(a => Console.WriteLine(a));
        }
    }
}

﻿using System;
using System.Linq;
using System.Reactive;
using System.Reactive.Linq;
using System.Collections.Immutable;
using System.Diagnostics.Contracts;

namespace Reactive
{
    // Syntaxic sugar
    [Pure]
    static class Extensions
    {
        public static Func<T, T> Identity<T>() =>
            x => x;
        public static Func<T, V> Compose<T, U, V>(this Func<U, V> g, Func<T, U> f) =>
            x => g(f(x));
        public static Func<U, T, V> Flip<T, U, V>(this Func<T, U, V> f) =>
            (x, y) => f(y, x);
        public static IObservable<Func<T, T>> FullTransformationMonoid<T>(this IObservable<Func<T, T>> morphismStream) =>
            morphismStream.Scan(Identity<T>(), Flip<Func<T, T>, Func<T, T>, Func<T, T>>(Compose));
    }

    enum EventType
    {
        HolyCow,
        FairyDrake,
        Unicorn
    }

    [Pure]
    class Event
    {
        public EventType Type { get; }
        public Event(EventType type) => Type = type;
    }

    // Fully immutable
    [Pure]
    class State
    {
        public static State Empty =>
            new State(0, ImmutableList<String>.Empty, String.Empty);

        public int Cow { get; }
        public ImmutableList<String> Drake { get; }
        public String Unicorn { get; }

        public State(int cow, ImmutableList<String> drake, String unicorn)
        {
            Cow = cow;
            Drake = drake;
            Unicorn = unicorn;
        }

        public override String ToString() =>
            $"cow={Cow}, drake={Drake.Count}, unicorn={Unicorn}";
    }

    class Program
    {
        // Imagine a flow comming from different sources and being merged
        [Pure]
        static IObservable<Event> EventStream() =>
            new []
            {
                new Event(EventType.Unicorn),
                new Event(EventType.FairyDrake),
                new Event(EventType.Unicorn),
                new Event(EventType.HolyCow),
                new Event(EventType.FairyDrake)
            }.ToObservable();

        // Given an event category, map each event to a morphism in our single object category
        [Pure]
        static IObservable<Func<State, State>> StateMorphismStream(IObservable<Event> eventStream) =>
            // Pretty sad but the compiler cannot infer high order functions...
            eventStream.Select<Event, Func<State, State>>(ev =>
                    {
                        // No pattern matching yet...
                        switch(ev.Type)
                        {
                            case EventType.HolyCow: return a => new State(a.Cow + 1, a.Drake, a.Unicorn);
                            case EventType.FairyDrake: return a => new State(a.Cow, a.Drake.Add("drake"), a.Unicorn);
                            case EventType.Unicorn: return a => new State(a.Cow, a.Drake, a.Unicorn + " UNNNNNNICORN");
                            default: return Extensions.Identity<State>();
                        }
                    });

        // Given a set T where objects are morphisms T -> T, it forms a monoid under function composition.
        // This type of monoid is called 'The full transformation monoid of T'
        // In our case, T = State and morphisms are functions State -> State generated by incomming Events
        [Pure]
        static IObservable<State> StateStream() =>
            StateMorphismStream(EventStream()).FullTransformationMonoid().Select(f => f(State.Empty));

        // Immutable, side-effect free code, easysn't it testable ?
        static void Main(string[] args) =>
            StateStream().Subscribe(a => Console.WriteLine(a));
    }
}

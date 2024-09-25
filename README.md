# `Janiczek/elm-priority-queue`

Priority queue is a collection that gives an easy access to its smallest (or largest) element.

---

This package is a fork of [`fifth-postulate/priority-queue`](https://package.elm-lang.org/packages/fifth-postulate/priority-queue/1.0.0/PriorityQueue) that:

* makes it easier (by naming the types explicitly) to know which "direction" the queue goes (items with smaller priority Ints first, or the other way round)
* contains a more complete API (eg. `singleton`, `filter`, `length`, `any`, etc.) 
* doesn't hold a function inside the data structure (thus is usable in the Elm debugger etc.)


```elm
import MaxPriorityQueue exposing (MaxPriorityQueue)

type alias BuyOffer =
    { amount : Int
    , unitPrice : Int
    }

offersByPrice : MaxPriorityQueue BuyOffer
offersByPrice =
    MaxPriorityQueue.fromList .unitPrice
        [ { amount = 10, unitPrice = 3000 }
        , { amount = 15, unitPrice = 10000 }
        , { amount = 100, unitPrice = 2000 }
        ]

bestOffer =
    MaxPriorityQueue.largest offersByPrice
    --> Just { amount = 1, unitPrice = 10000 }

bestAndRest =
    MaxPriorityQueue.dequeue offersByPrice
    --> Just
    --    ( { amount = 15, unitPrice = 10000 }
    --    , MaxPriorityQueue.fromList .unitPrice
    --        [ { amount = 10, unitPrice = 3000 }
    --        , { amount = 100, unitPrice = 2000 }
    --        ]
    --    )

nextBest =
    bestAndRest
        |> Maybe.andThen (\(_, rest) -> MaxPriorityQueue.largest rest)
        --> Just { amount = 10, unitPrice = 3000 }
```

`MinPriorityQueue` works similarly, just has `smallest` instead of `largest` in its functions:

```elm
import MinPriorityQueue exposing (MinPriorityQueue)

type alias Person =
    { name : String
    , age : Int
    }

peopleByAge : MinPriorityQueue Person
peopleByAge =
    MinPriorityQueue.fromList .age
        [ Person "Martin" 31
        , Person "Xavier" 13
        , Person "Joanne" 54
        ]

youngest : Maybe Person
youngest =
    MinPriorityQueue.smallest peopleByAge
    --> Just { name = "Xavier", age = 13 }
```

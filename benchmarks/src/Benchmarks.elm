module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (rank)
import Benchmark.Runner.Alternative as BenchmarkRunner
import V1_1_0.MinPriorityQueue
import V1_1_1.MinPriorityQueue


main : BenchmarkRunner.Program
main =
    describe "Performance changes"
        [ insertMany
        , toList
        , toSortedList
        ]
        |> BenchmarkRunner.program


insertMany : Benchmark
insertMany =
    describe "Insert many"
        [ rank "Priority equals n"
            (\fn -> fn ())
            [ ( "1.1.1"
              , \() ->
                    List.foldl (\n queue -> V1_1_1.MinPriorityQueue.insert identity n queue) V1_1_1.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            , ( "1.1.0"
              , \() ->
                    List.foldl (\n queue -> V1_1_0.MinPriorityQueue.insert identity n queue) V1_1_0.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "1.1.1"
              , \() ->
                    List.foldl (\n queue -> V1_1_1.MinPriorityQueue.insert always0 n queue) V1_1_1.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            , ( "1.1.0"
              , \() ->
                    List.foldl (\n queue -> V1_1_0.MinPriorityQueue.insert always0 n queue) V1_1_0.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            ]
        ]


toList : Benchmark
toList =
    describe "toList"
        [ rank "Priority equals n"
            (\fn -> fn ())
            [ ( "1.1.1", \() -> V1_1_1.MinPriorityQueue.toList queueWithDifferentPrioritiesV1_1_1 )
            , ( "1.1.0", \() -> V1_1_0.MinPriorityQueue.toList queueWithDifferentPrioritiesV1_1_0 )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "1.1.1", \() -> V1_1_1.MinPriorityQueue.toList queueWithSamePrioritiesV1_1_1 )
            , ( "1.1.0", \() -> V1_1_0.MinPriorityQueue.toList queueWithSamePrioritiesV1_1_0 )
            ]
        ]


toSortedList : Benchmark
toSortedList =
    describe "toSortedList"
        [ rank "Priority equals n"
            (\fn -> fn ())
            [ ( "1.1.1", \() -> V1_1_1.MinPriorityQueue.toSortedList queueWithDifferentPrioritiesV1_1_1 )
            , ( "1.1.0", \() -> V1_1_0.MinPriorityQueue.toSortedList queueWithDifferentPrioritiesV1_1_0 )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "1.1.1", \() -> V1_1_1.MinPriorityQueue.toSortedList queueWithSamePrioritiesV1_1_1 )
            , ( "1.1.0", \() -> V1_1_0.MinPriorityQueue.toSortedList queueWithSamePrioritiesV1_1_0 )
            ]
        ]


queueWithDifferentPrioritiesV1_1_1 : V1_1_1.MinPriorityQueue.MinPriorityQueue Int
queueWithDifferentPrioritiesV1_1_1 =
    V1_1_1.MinPriorityQueue.fromList identity thousandItems


queueWithDifferentPrioritiesV1_1_0 : V1_1_0.MinPriorityQueue.MinPriorityQueue Int
queueWithDifferentPrioritiesV1_1_0 =
    V1_1_0.MinPriorityQueue.fromList identity thousandItems


queueWithSamePrioritiesV1_1_1 : V1_1_1.MinPriorityQueue.MinPriorityQueue Int
queueWithSamePrioritiesV1_1_1 =
    V1_1_1.MinPriorityQueue.fromList always0 thousandItems


queueWithSamePrioritiesV1_1_0 : V1_1_0.MinPriorityQueue.MinPriorityQueue Int
queueWithSamePrioritiesV1_1_0 =
    V1_1_0.MinPriorityQueue.fromList always0 thousandItems


thousandItems : List Int
thousandItems =
    List.range 0 1000


always0 : a -> Int
always0 =
    always 0

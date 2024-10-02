module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (rank)
import Benchmark.Runner.Alternative as BenchmarkRunner
import MinPriorityQueue
import Vendor.MinPriorityQueue


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
            [ ( "Current"
              , \() ->
                    List.foldl (\n queue -> MinPriorityQueue.insert identity n queue) MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            , ( "Previous"
              , \() ->
                    List.foldl (\n queue -> Vendor.MinPriorityQueue.insert identity n queue) Vendor.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "Current"
              , \() ->
                    List.foldl (\n queue -> MinPriorityQueue.insert always0 n queue) MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            , ( "Previous"
              , \() ->
                    List.foldl (\n queue -> Vendor.MinPriorityQueue.insert always0 n queue) Vendor.MinPriorityQueue.empty thousandItems
                        |> always ()
              )
            ]
        ]


toList : Benchmark
toList =
    describe "toList"
        [ rank "Priority equals n"
            (\fn -> fn ())
            [ ( "Current", \() -> MinPriorityQueue.toList queueWithDifferentPrioritiesCurrent )
            , ( "Previous", \() -> Vendor.MinPriorityQueue.toList queueWithDifferentPrioritiesPrevious )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "Current", \() -> MinPriorityQueue.toList queueWithSamePrioritiesCurrent )
            , ( "Previous", \() -> Vendor.MinPriorityQueue.toList queueWithSamePrioritiesPrevious )
            ]
        ]


toSortedList : Benchmark
toSortedList =
    describe "toSortedList"
        [ rank "Priority equals n"
            (\fn -> fn ())
            [ ( "Current", \() -> MinPriorityQueue.toSortedList queueWithDifferentPrioritiesCurrent )
            , ( "Previous", \() -> Vendor.MinPriorityQueue.toSortedList queueWithDifferentPrioritiesPrevious )
            ]
        , rank "Priority is always the same"
            (\fn -> fn ())
            [ ( "Current", \() -> MinPriorityQueue.toSortedList queueWithSamePrioritiesCurrent )
            , ( "Previous", \() -> Vendor.MinPriorityQueue.toSortedList queueWithSamePrioritiesPrevious )
            ]
        ]


queueWithDifferentPrioritiesCurrent : MinPriorityQueue.MinPriorityQueue Int
queueWithDifferentPrioritiesCurrent =
    MinPriorityQueue.fromList identity thousandItems


queueWithDifferentPrioritiesPrevious : Vendor.MinPriorityQueue.MinPriorityQueue Int
queueWithDifferentPrioritiesPrevious =
    Vendor.MinPriorityQueue.fromList identity thousandItems


queueWithSamePrioritiesCurrent : MinPriorityQueue.MinPriorityQueue Int
queueWithSamePrioritiesCurrent =
    MinPriorityQueue.fromList always0 thousandItems


queueWithSamePrioritiesPrevious : Vendor.MinPriorityQueue.MinPriorityQueue Int
queueWithSamePrioritiesPrevious =
    Vendor.MinPriorityQueue.fromList always0 thousandItems


thousandItems : List Int
thousandItems =
    List.range 0 1000


always0 : a -> Int
always0 =
    always 0

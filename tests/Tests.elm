module Tests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import MaxPriorityQueue exposing (MaxPriorityQueue)
import MinPriorityQueue exposing (MinPriorityQueue)
import Test exposing (Test)


minPriorityQueueFuzzer : Fuzzer (MinPriorityQueue Int)
minPriorityQueueFuzzer =
    Fuzz.list Fuzz.int
        |> Fuzz.map (MinPriorityQueue.fromList identity)


maxPriorityQueueFuzzer : Fuzzer (MaxPriorityQueue Int)
maxPriorityQueueFuzzer =
    Fuzz.list Fuzz.int
        |> Fuzz.map (MaxPriorityQueue.fromList identity)


suite : Test
suite =
    Test.describe "Priority Queue Tests"
        [ minPriorityQueueSuite
        , maxPriorityQueueSuite
        ]


minPriorityQueueSuite : Test
minPriorityQueueSuite =
    Test.describe "MinPriorityQueue"
        [ Test.fuzz (Fuzz.list (Fuzz.intRange 100 200)) "[smallest] example with known smallest" <|
            \randomInts ->
                let
                    queue : MinPriorityQueue Int
                    queue =
                        MinPriorityQueue.fromList identity (42 :: randomInts)
                in
                MinPriorityQueue.smallest queue
                    |> Expect.equal (Just 42)
        , Test.fuzz minPriorityQueueFuzzer "smallest == head" <|
            \queue ->
                MinPriorityQueue.smallest queue
                    |> Expect.equal (MinPriorityQueue.head queue)
        , Test.fuzz (Fuzz.list (Fuzz.intRange 100 200)) "[drop] example with known smallest items" <|
            \randomInts ->
                ([ 1, 2, 3, 4, 5 ] ++ randomInts)
                    |> MinPriorityQueue.fromList identity
                    |> MinPriorityQueue.drop 5
                    |> MinPriorityQueue.all (\x -> x > 5)
                    |> Expect.equal True
                    |> Expect.onFail "Expected all remaining items to be greater than 5"
        , Test.test "smallest empty == Nothing" <|
            \_ ->
                MinPriorityQueue.empty
                    |> MinPriorityQueue.smallest
                    |> Expect.equal Nothing
        , Test.fuzz Fuzz.int "smallest (singleton x) == Just x" <|
            \int ->
                let
                    singletonQueue : MinPriorityQueue Int
                    singletonQueue =
                        MinPriorityQueue.singleton identity int
                in
                MinPriorityQueue.smallest singletonQueue
                    |> Expect.equal (Just int)
        , Test.fuzz (Fuzz.list Fuzz.int) "smallest (fromList xs) == List.minimum xs" <|
            \randomInts ->
                let
                    queue : MinPriorityQueue Int
                    queue =
                        MinPriorityQueue.fromList identity randomInts
                in
                MinPriorityQueue.smallest queue
                    |> Expect.equal (List.minimum randomInts)
        , Test.fuzz (Fuzz.list Fuzz.int) "sort (toList (fromList xs)) == sort xs" <|
            \randomInts ->
                let
                    roundTripped : List Int
                    roundTripped =
                        randomInts
                            |> MinPriorityQueue.fromList identity
                            |> MinPriorityQueue.toList
                in
                List.sort roundTripped
                    |> Expect.equalLists (List.sort randomInts)
                    |> Expect.onFail "Expected fromList -> toList to maintain the items"
        , Test.fuzz (Fuzz.list Fuzz.int) "reverseSort (toList xs) == toSortedList xs" <|
            \randomInts ->
                let
                    queue : MinPriorityQueue Int
                    queue =
                        MinPriorityQueue.fromList identity randomInts
                in
                (queue |> MinPriorityQueue.toSortedList)
                    |> Expect.equalLists (queue |> MinPriorityQueue.toList |> List.sortBy negate)
                    |> Expect.onFail "Expected sort (toList xs) to be equal to toSortedList xs"
        , Test.test "toList empty == []" <|
            \_ ->
                MinPriorityQueue.toList MinPriorityQueue.empty
                    |> Expect.equal []
                    |> Expect.onFail "Expected toList of an empty queue to be an empty list"
        , Test.fuzz Fuzz.int "toList (singleton x) == [x]" <|
            \int ->
                let
                    singletonQueue : MinPriorityQueue Int
                    singletonQueue =
                        MinPriorityQueue.singleton identity int
                in
                MinPriorityQueue.toList singletonQueue
                    |> Expect.equal [ int ]
                    |> Expect.onFail "Expected toList of a singleton queue to be a list with one element"
        , Test.fuzz Fuzz.int "singleton x == insert x empty" <|
            \element ->
                let
                    insertedQueue : MinPriorityQueue Int
                    insertedQueue =
                        MinPriorityQueue.empty
                            |> MinPriorityQueue.insert identity element

                    singletonQueue : MinPriorityQueue Int
                    singletonQueue =
                        MinPriorityQueue.singleton identity element
                in
                MinPriorityQueue.toList insertedQueue
                    |> Expect.equal (MinPriorityQueue.toList singletonQueue)
                    |> Expect.onFail "Expected inserting into an empty queue to be equivalent to creating a singleton queue"
        , Test.fuzz minPriorityQueueFuzzer "filter (always True) xs == xs" <|
            \queue ->
                let
                    filteredQueue : MinPriorityQueue Int
                    filteredQueue =
                        MinPriorityQueue.filter (always True) queue

                    originalList : List Int
                    originalList =
                        MinPriorityQueue.toList queue

                    filteredList : List Int
                    filteredList =
                        MinPriorityQueue.toList filteredQueue
                in
                filteredList
                    |> Expect.equalLists originalList
                    |> Expect.onFail "Expected filtering with (always True) to not change the queue"
        , Test.fuzz minPriorityQueueFuzzer "filter (always False) xs == empty" <|
            \queue ->
                let
                    filteredQueue : MinPriorityQueue Int
                    filteredQueue =
                        MinPriorityQueue.filter (always False) queue
                in
                MinPriorityQueue.isEmpty filteredQueue
                    |> Expect.equal True
                    |> Expect.onFail "Expected filtering with (always False) to result in an empty queue"
        , Test.fuzz minPriorityQueueFuzzer "dequeue == head + tail" <|
            \queue ->
                let
                    dequeueResult : Maybe ( Int, MinPriorityQueue Int )
                    dequeueResult =
                        MinPriorityQueue.dequeue queue

                    headAndTailResult : Maybe ( Int, MinPriorityQueue Int )
                    headAndTailResult =
                        Maybe.map2 Tuple.pair
                            (MinPriorityQueue.head queue)
                            (MinPriorityQueue.tail queue)
                in
                Expect.equal dequeueResult headAndTailResult
                    |> Expect.onFail "Expected dequeue to be equivalent to (head, tail)"
        , Test.fuzz2 (Fuzz.intRange 0 10) minPriorityQueueFuzzer "reverseSort (take n xs) == take n xs" <|
            \n queue ->
                let
                    takenList : List Int
                    takenList =
                        MinPriorityQueue.take n queue

                    sortedTakenList : List Int
                    sortedTakenList =
                        List.sortBy negate takenList
                in
                sortedTakenList
                    |> Expect.equalLists takenList
                    |> Expect.onFail "Expected the taken elements to already be sorted"
        , Test.fuzz2 (Fuzz.intRange 0 100) minPriorityQueueFuzzer "take (length xs + n) == take (length xs)" <|
            \n queue ->
                let
                    queueLength : Int
                    queueLength =
                        MinPriorityQueue.length queue

                    takeAll : List Int
                    takeAll =
                        MinPriorityQueue.take queueLength queue

                    takeMore : List Int
                    takeMore =
                        MinPriorityQueue.take (queueLength + n) queue
                in
                takeMore
                    |> Expect.equalLists takeAll
                    |> Expect.onFail "Expected taking more than the queue length to be equivalent to taking exactly the queue length"
        , Test.fuzz minPriorityQueueFuzzer "all (always True) xs == True" <|
            \queue ->
                MinPriorityQueue.all (always True) queue
                    |> Expect.equal True
                    |> Expect.onFail "Expected all (always True) to be True for any queue"
        , Test.test "all (always False) empty == True" <|
            \() ->
                MinPriorityQueue.all (always False) MinPriorityQueue.empty
                    |> Expect.equal True
                    |> Expect.onFail "Expected all (always False) to be True for empty queue"
        , Test.fuzz minPriorityQueueFuzzer "all (always False) non-empty == False" <|
            \queue ->
                if MinPriorityQueue.isEmpty queue then
                    Expect.pass

                else
                    MinPriorityQueue.all (always False) queue
                        |> Expect.equal False
                        |> Expect.onFail "Expected all (always False) to be False for non-empty queue"
        , Test.fuzz minPriorityQueueFuzzer "any (always True) non-empty == True" <|
            \queue ->
                if MinPriorityQueue.isEmpty queue then
                    Expect.pass

                else
                    MinPriorityQueue.any (always True) queue
                        |> Expect.equal True
                        |> Expect.onFail "Expected any (always True) to be True for any queue"
        , Test.test "any (always True) empty == False" <|
            \() ->
                MinPriorityQueue.any (always False) MinPriorityQueue.empty
                    |> Expect.equal False
                    |> Expect.onFail "Expected any (always False) to be True for empty queue"
        , Test.fuzz minPriorityQueueFuzzer "any (always False) xs == False" <|
            \queue ->
                MinPriorityQueue.any (always False) queue
                    |> Expect.equal False
                    |> Expect.onFail "Expected any (always False) to be False for non-empty queue"
        , Test.test "any is stack safe when all elements have a different priority" <|
            \() ->
                MinPriorityQueue.fromList identity (List.range 0 10000)
                    |> MinPriorityQueue.any (always False)
                    |> Expect.equal False
        , Test.test "any is stack safe even when all elements have the same priority" <|
            \() ->
                MinPriorityQueue.fromList (always 0) (List.range 0 10000)
                    |> MinPriorityQueue.any (always False)
                    |> Expect.equal False
        , Test.fuzz minPriorityQueueFuzzer "isEmpty xs <=> length xs == 0" <|
            \queue ->
                let
                    isEmpty : Bool
                    isEmpty =
                        MinPriorityQueue.isEmpty queue

                    lengthIsZero : Bool
                    lengthIsZero =
                        MinPriorityQueue.length queue == 0
                in
                Expect.equal isEmpty lengthIsZero
                    |> Expect.onFail "Expected isEmpty to be equivalent to length == 0"
        , Test.fuzz2 minPriorityQueueFuzzer Fuzz.int "enqueue == insert" <|
            \queue element ->
                let
                    enqueueResult : MinPriorityQueue Int
                    enqueueResult =
                        MinPriorityQueue.enqueue identity element queue

                    insertResult : MinPriorityQueue Int
                    insertResult =
                        MinPriorityQueue.insert identity element queue
                in
                MinPriorityQueue.toList enqueueResult
                    |> Expect.equal (MinPriorityQueue.toList insertResult)
                    |> Expect.onFail "Expected enqueue to be equivalent to insert"
        , Test.fuzz2 minPriorityQueueFuzzer (Fuzz.intRange 0 10) "dequeueMany == take, drop" <|
            \queue n ->
                let
                    dequeueManyResult : ( List Int, MinPriorityQueue Int )
                    dequeueManyResult =
                        MinPriorityQueue.dequeueMany n queue

                    takeResult : List Int
                    takeResult =
                        MinPriorityQueue.take n queue

                    dropResult : MinPriorityQueue Int
                    dropResult =
                        MinPriorityQueue.drop n queue
                in
                Expect.all
                    [ \_ ->
                        Tuple.first dequeueManyResult
                            |> Expect.equal takeResult
                            |> Expect.onFail "Expected dequeueMany's first tuple element to equal take result"
                    , \_ ->
                        Tuple.second dequeueManyResult
                            |> MinPriorityQueue.toList
                            |> Expect.equal (MinPriorityQueue.toList dropResult)
                            |> Expect.onFail "Expected dequeueMany's second tuple element to equal drop result"
                    ]
                    ()
        , Test.fuzz (Fuzz.intRange 0 100) "drop n empty == empty" <|
            \n ->
                let
                    emptyQueue : MinPriorityQueue Int
                    emptyQueue =
                        MinPriorityQueue.empty

                    droppedQueue : MinPriorityQueue Int
                    droppedQueue =
                        MinPriorityQueue.drop n emptyQueue
                in
                MinPriorityQueue.isEmpty droppedQueue
                    |> Expect.equal True
                    |> Expect.onFail ("Expected dropping " ++ String.fromInt n ++ " elements from an empty queue to result in an empty queue")
        ]


maxPriorityQueueSuite : Test
maxPriorityQueueSuite =
    Test.describe "MaxPriorityQueue"
        [ Test.fuzz (Fuzz.list (Fuzz.intRange 1 10)) "[largest] example with known largest" <|
            \randomInts ->
                let
                    queue : MaxPriorityQueue Int
                    queue =
                        MaxPriorityQueue.fromList identity (42 :: randomInts)
                in
                MaxPriorityQueue.largest queue
                    |> Expect.equal (Just 42)
        , Test.fuzz maxPriorityQueueFuzzer "largest == head" <|
            \queue ->
                MaxPriorityQueue.largest queue
                    |> Expect.equal (MaxPriorityQueue.head queue)
        , Test.fuzz (Fuzz.list (Fuzz.intRange 1 9)) "[drop] example with known largest items" <|
            \randomInts ->
                ([ 10, 20, 30, 40, 50 ] ++ randomInts)
                    |> MaxPriorityQueue.fromList identity
                    |> MaxPriorityQueue.drop 5
                    |> MaxPriorityQueue.all (\x -> x < 10)
                    |> Expect.equal True
                    |> Expect.onFail "Expected all remaining items to be smaller than 10"
        , Test.test "all is stack safe when all elements have a different priority" <|
            \() ->
                MinPriorityQueue.fromList identity (List.range 0 10000)
                    |> MinPriorityQueue.all (always True)
                    |> Expect.equal True
        , Test.test "all is stack safe even when all elements have the same priority" <|
            \() ->
                MinPriorityQueue.fromList (always 0) (List.range 0 10000)
                    |> MinPriorityQueue.all (always True)
                    |> Expect.equal True
        , Test.test "largest empty == Nothing" <|
            \_ ->
                MaxPriorityQueue.empty
                    |> MaxPriorityQueue.largest
                    |> Expect.equal Nothing
        , Test.fuzz Fuzz.int "largest (singleton x) == Just x" <|
            \int ->
                let
                    singletonQueue : MaxPriorityQueue Int
                    singletonQueue =
                        MaxPriorityQueue.singleton identity int
                in
                MaxPriorityQueue.largest singletonQueue
                    |> Expect.equal (Just int)
        , Test.fuzz (Fuzz.list Fuzz.int) "largest (fromList xs) == List.maximum xs" <|
            \randomInts ->
                let
                    queue : MaxPriorityQueue Int
                    queue =
                        MaxPriorityQueue.fromList identity randomInts
                in
                MaxPriorityQueue.largest queue
                    |> Expect.equal (List.maximum randomInts)
        , Test.fuzz (Fuzz.list Fuzz.int) "sort (toList (fromList xs)) == sort xs" <|
            \randomInts ->
                let
                    roundTripped : List Int
                    roundTripped =
                        randomInts
                            |> MaxPriorityQueue.fromList identity
                            |> MaxPriorityQueue.toList
                in
                List.sort roundTripped
                    |> Expect.equalLists (List.sort randomInts)
                    |> Expect.onFail "Expected fromList -> toList to maintain the items"
        , Test.fuzz (Fuzz.list Fuzz.int) "sort (toList xs) == toSortedList xs" <|
            \randomInts ->
                let
                    queue : MaxPriorityQueue Int
                    queue =
                        MaxPriorityQueue.fromList identity randomInts
                in
                (queue |> MaxPriorityQueue.toSortedList)
                    |> Expect.equalLists (queue |> MaxPriorityQueue.toList |> List.sort)
                    |> Expect.onFail "Expected sort (toList xs) to be equal to toSortedList xs"
        , Test.test "toList empty == []" <|
            \_ ->
                MaxPriorityQueue.toList MaxPriorityQueue.empty
                    |> Expect.equal []
                    |> Expect.onFail "Expected toList of an empty queue to be an empty list"
        , Test.fuzz Fuzz.int "toList (singleton x) == [x]" <|
            \int ->
                let
                    singletonQueue : MaxPriorityQueue Int
                    singletonQueue =
                        MaxPriorityQueue.singleton identity int
                in
                MaxPriorityQueue.toList singletonQueue
                    |> Expect.equal [ int ]
                    |> Expect.onFail "Expected toList of a singleton queue to be a list with one element"
        , Test.fuzz Fuzz.int "singleton x == insert x empty" <|
            \element ->
                let
                    insertedQueue : MaxPriorityQueue Int
                    insertedQueue =
                        MaxPriorityQueue.empty
                            |> MaxPriorityQueue.insert identity element

                    singletonQueue : MaxPriorityQueue Int
                    singletonQueue =
                        MaxPriorityQueue.singleton identity element
                in
                MaxPriorityQueue.toList insertedQueue
                    |> Expect.equal (MaxPriorityQueue.toList singletonQueue)
                    |> Expect.onFail "Expected inserting into an empty queue to be equivalent to creating a singleton queue"
        , Test.fuzz maxPriorityQueueFuzzer "filter (always True) xs == xs" <|
            \queue ->
                let
                    filteredQueue : MaxPriorityQueue Int
                    filteredQueue =
                        MaxPriorityQueue.filter (always True) queue

                    originalList : List Int
                    originalList =
                        MaxPriorityQueue.toList queue

                    filteredList : List Int
                    filteredList =
                        MaxPriorityQueue.toList filteredQueue
                in
                filteredList
                    |> Expect.equalLists originalList
                    |> Expect.onFail "Expected filtering with (always True) to not change the queue"
        , Test.fuzz maxPriorityQueueFuzzer "filter (always False) xs == empty" <|
            \queue ->
                let
                    filteredQueue : MaxPriorityQueue Int
                    filteredQueue =
                        MaxPriorityQueue.filter (always False) queue
                in
                MaxPriorityQueue.isEmpty filteredQueue
                    |> Expect.equal True
                    |> Expect.onFail "Expected filtering with (always False) to result in an empty queue"
        , Test.fuzz maxPriorityQueueFuzzer "dequeue == head + tail" <|
            \queue ->
                let
                    dequeueResult : Maybe ( Int, MaxPriorityQueue Int )
                    dequeueResult =
                        MaxPriorityQueue.dequeue queue

                    headAndTailResult : Maybe ( Int, MaxPriorityQueue Int )
                    headAndTailResult =
                        Maybe.map2 Tuple.pair
                            (MaxPriorityQueue.head queue)
                            (MaxPriorityQueue.tail queue)
                in
                Expect.equal dequeueResult headAndTailResult
                    |> Expect.onFail "Expected dequeue to be equivalent to (head, tail)"
        , Test.fuzz2 (Fuzz.intRange 0 10) maxPriorityQueueFuzzer "sort (take n xs) == take n xs" <|
            \n queue ->
                let
                    takenList : List Int
                    takenList =
                        MaxPriorityQueue.take n queue

                    sortedTakenList : List Int
                    sortedTakenList =
                        List.sort takenList
                in
                sortedTakenList
                    |> Expect.equalLists takenList
                    |> Expect.onFail "Expected the taken elements to already be sorted"
        , Test.fuzz2 (Fuzz.intRange 0 100) maxPriorityQueueFuzzer "take (length xs + n) == take (length xs)" <|
            \n queue ->
                let
                    queueLength : Int
                    queueLength =
                        MaxPriorityQueue.length queue

                    takeAll : List Int
                    takeAll =
                        MaxPriorityQueue.take queueLength queue

                    takeMore : List Int
                    takeMore =
                        MaxPriorityQueue.take (queueLength + n) queue
                in
                takeMore
                    |> Expect.equalLists takeAll
                    |> Expect.onFail "Expected taking more than the queue length to be equivalent to taking exactly the queue length"
        , Test.fuzz maxPriorityQueueFuzzer "all (always True) xs == True" <|
            \queue ->
                MaxPriorityQueue.all (always True) queue
                    |> Expect.equal True
                    |> Expect.onFail "Expected all (always True) to be True for any queue"
        , Test.test "all (always False) empty == True" <|
            \() ->
                MaxPriorityQueue.all (always False) MaxPriorityQueue.empty
                    |> Expect.equal True
                    |> Expect.onFail "Expected all (always False) to be True for empty queue"
        , Test.fuzz maxPriorityQueueFuzzer "all (always False) non-empty == False" <|
            \queue ->
                if MaxPriorityQueue.isEmpty queue then
                    Expect.pass

                else
                    MaxPriorityQueue.all (always False) queue
                        |> Expect.equal False
                        |> Expect.onFail "Expected all (always False) to be False for non-empty queue"
        , Test.fuzz maxPriorityQueueFuzzer "any (always True) non-empty == True" <|
            \queue ->
                if MaxPriorityQueue.isEmpty queue then
                    Expect.pass

                else
                    MaxPriorityQueue.any (always True) queue
                        |> Expect.equal True
                        |> Expect.onFail "Expected any (always True) to be True for any queue"
        , Test.test "any (always True) empty == False" <|
            \() ->
                MaxPriorityQueue.any (always False) MaxPriorityQueue.empty
                    |> Expect.equal False
                    |> Expect.onFail "Expected any (always False) to be True for empty queue"
        , Test.fuzz maxPriorityQueueFuzzer "any (always False) xs == False" <|
            \queue ->
                MaxPriorityQueue.any (always False) queue
                    |> Expect.equal False
                    |> Expect.onFail "Expected any (always False) to be False for non-empty queue"
        , Test.fuzz maxPriorityQueueFuzzer "isEmpty xs <=> length xs == 0" <|
            \queue ->
                let
                    isEmpty : Bool
                    isEmpty =
                        MaxPriorityQueue.isEmpty queue

                    lengthIsZero : Bool
                    lengthIsZero =
                        MaxPriorityQueue.length queue == 0
                in
                Expect.equal isEmpty lengthIsZero
                    |> Expect.onFail "Expected isEmpty to be equivalent to length == 0"
        , Test.fuzz2 maxPriorityQueueFuzzer Fuzz.int "enqueue == insert" <|
            \queue element ->
                let
                    enqueueResult : MaxPriorityQueue Int
                    enqueueResult =
                        MaxPriorityQueue.enqueue identity element queue

                    insertResult : MaxPriorityQueue Int
                    insertResult =
                        MaxPriorityQueue.insert identity element queue
                in
                MaxPriorityQueue.toList enqueueResult
                    |> Expect.equal (MaxPriorityQueue.toList insertResult)
                    |> Expect.onFail "Expected enqueue to be equivalent to insert"
        , Test.fuzz2 maxPriorityQueueFuzzer (Fuzz.intRange 0 10) "dequeueMany == take, drop" <|
            \queue n ->
                let
                    dequeueManyResult : ( List Int, MaxPriorityQueue Int )
                    dequeueManyResult =
                        MaxPriorityQueue.dequeueMany n queue

                    takeResult : List Int
                    takeResult =
                        MaxPriorityQueue.take n queue

                    dropResult : MaxPriorityQueue Int
                    dropResult =
                        MaxPriorityQueue.drop n queue
                in
                Expect.all
                    [ \_ ->
                        Tuple.first dequeueManyResult
                            |> Expect.equal takeResult
                            |> Expect.onFail "Expected dequeueMany's first tuple element to equal take result"
                    , \_ ->
                        Tuple.second dequeueManyResult
                            |> MaxPriorityQueue.toList
                            |> Expect.equal (MaxPriorityQueue.toList dropResult)
                            |> Expect.onFail "Expected dequeueMany's second tuple element to equal drop result"
                    ]
                    ()
        , Test.fuzz (Fuzz.intRange 0 100) "drop n empty == empty" <|
            \n ->
                let
                    emptyQueue : MaxPriorityQueue Int
                    emptyQueue =
                        MaxPriorityQueue.empty

                    droppedQueue : MaxPriorityQueue Int
                    droppedQueue =
                        MaxPriorityQueue.drop n emptyQueue
                in
                MaxPriorityQueue.isEmpty droppedQueue
                    |> Expect.equal True
                    |> Expect.onFail ("Expected dropping " ++ String.fromInt n ++ " elements from an empty queue to result in an empty queue")
        ]

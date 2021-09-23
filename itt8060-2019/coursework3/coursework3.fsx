(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher-order functions

  ------------------------------------
  Name: Vladyslav Umerenko
  Tallinn University of Technology Student ID: vlumer
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2019 under your name, into a file coursework3/coursework3.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Your solution should not contain the keywords 'rec' and 'mutable'.

  Deadline for submission is Sunday, October 6.
*)


// A list can contain items of the same type.
// We use a Cell to collect things that can be slightly different.

type Cell = Empty | Value of int | Pair of (int * int)




// 1. Define a function
//
// noEmptyCells : Cell list -> Cell list
//
// that discards all cells that are Empty.
//
// Use List.filter

let noEmptyCells (xs : Cell list) =
  xs |> List.filter (fun x -> match x with | Empty -> false | _ -> true)


// 2. Define a function
//
// filterGreaterThan : int -> Cell list -> Cell list
//
// that discards all cells with value less than or equal to n.
// This means that all Empty cells should be discarded,
// Value v should be discarded when v is less than or equal to n,
// Pair (x, y) should be discarded when both x and y are less than or equal to n.
//
// Use List.filter

let filterGreaterThan (n : int) (xs : Cell list) =
  let greaterThan (cell : Cell) =
    match cell with
    | Empty -> false
    | Value c when c <= n -> false
    | Pair (x, y) when x <= n && y <= n -> false
    | _ -> true

  xs |> List.filter greaterThan


// 3. Define a function
//
// increaseCells : int -> Cell list -> Cell list
//
// that increases the values in the cells by the given amount.
// Empty should stay Empty and for Pairs you should increase
// both of the values in the pair.
//
// Use List.map

let increaseCells (n : int) (xs : Cell list) =
  let increase (x : Cell) =
    match x with
    | Empty -> Empty
    | Value v -> Value (v + n)
    | Pair (x, y) -> Pair (x + n, y + n)


  xs |> List.map increase


// 4. Define a function
//
// transformPairs : (int -> int -> Cell) -> Cell list -> Cell list
//
// that replaces the Pair cells in the list
// with the result of applying the given operation to the two integer values in the pair.
//
// 'transformPairs f xs' should replace a 'Pair (x, y)' with 'f x y'.
//
// Use List.map

let transformPairs (f : int -> int -> Cell) (xs : Cell list) =
  let transform (x : Cell) =
    match x with
    | Pair (x, y) -> f x y
    | _ -> x

  xs |> List.map transform


// 5. Define a function
//
// pairsToEmpty : Cell list -> Cell list
//
// that replaces all Pairs with Empty cells.

let pairsToEmpty (xs : Cell list) =
  let replace (x : Cell) =
    match x with
    | Pair _ -> Empty
    | _ -> x

  xs |> List.map replace


// 6. Define a function
//
// replicateCells : Cell list -> Cell list
//
// that replicates each cell in the list n times, where n is
// 0             for Empty
// max 0 v       for Value v
// max 0 (x + y) for Pair (x, y)
//
// Use List.collect

let replicateCells (xs : Cell list) =
  let replicate (x : Cell) =
    match x with
    | Empty -> []
    | Value v -> List.replicate (max 0 v) (Value v)
    | Pair (x, y) -> List.replicate (max 0 (x + y)) (Pair (x, y))

  xs |> List.collect replicate


// 7. Define a function
//
// flattenPairs : Cell list -> Cell list
//
// that replaces a Pair (x, y) with Value x followed by Value y.
//
// Use List.collect.

let flattenPairs (xs : Cell list) =
  let flatten (x : Cell) =
    match x with
    | Empty -> [Empty]
    | Value v -> [Value v]
    | Pair (x, y) -> [Value x; Value y]

  xs |> List.collect flatten


// 8. Define a function
//
// countCells : Cell list -> int * int * int
//
// which counts the number of Empty, Value, and Pair cells in the list.
//
// If 'countCells xs' is (1, 2, 3) then
// 1 is the number of Empty cells
// 2 is the number of Value cells
// 3 is the number of Pair  cells
//
// Use List.fold

let countCells (xs : Cell list) : (int * int * int) =
  let count (acc : (int * int * int)) (x : Cell) =
    match x, acc with
    | Empty, (e, v, p)   -> (e + 1, v, p)
    | Value _, (e, v, p) -> (e, v + 1, p)
    | Pair _, (e, v, p)  -> (e, v, p + 1)

  xs |> List.fold count (0, 0, 0)


// 9. Define a function
//
// cellsToString : Cell list -> string
//
// that constructs a string representation of the cells.
//
// Empty       is represented as "."
// Value v     is represented as "v"
// Pair (x, y) is represented as "x,y"
//
// Use "|" as the separator symbol.
// The result must start and end with the separator.
// You can convert an int to a string by the function 'string'.
//
// Use List.fold

let cellsToString (xs : Cell list) =
  let convert (acc : string) (x : Cell) =
    match x with
    | Empty -> acc + "." + "|"
    | Value v -> acc + (string v) + "|"
    | Pair (x, y) -> acc + (string x) + "," + (string y) + "|"

  "|" + (xs |> List.fold convert "")

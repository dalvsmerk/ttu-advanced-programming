(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 5: Tail recursion

  ------------------------------------------------
  Name: Vladyslav Umerenko
  Student ID: vlumer
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 3, 2019.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function
  forAllInList : (int -> int -> bool) -> (int * int) list ->  bool
  that returns true if the predicate passed as the first argument holds
  for all pairs in the list passed as the second argument to the function.
  Make sure your implementation uses explicit tail recursion.

  e.g.

  forAllInList (fun _ _ -> true) [(1,2);(3,4)] should return true.
*)

let rec forAllInList (pred : int -> int -> bool) (xs : (int * int) list) : bool =
  match xs with
  | [] -> true
  | xs -> forAll true pred xs
and forAll acc pred xs =
  match xs with
  | []               -> acc
  | (x1, x2) :: tail -> forAll (acc && (pred x1 x2)) pred tail


(*
  Task 2:

  Write a function
  filterAndCreatePairs : 'a -> ('a -> bool) -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s (as third argument), filters them with the predicate given as the second argument
  and returns a list of pairs of 'a-s that are taken
  from the start of the list passed as the second argument
  to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the last pair.
  Make sure your implementation uses explicit tail recursion.

  e.g. evaluating
  filterAndCreatePairs 0 (fun _ -> true) [1;2;3] should produce
  [(1,2);(3;0)]

  and evaluating
  createPairsOfList "a" (fun _ -> true) ["b";"c";"d";"e"] should produce
  [("b","c");("d","e")]
*)

let rec filterAndCreatePairs (def : 'a) (pred : 'a -> bool) (xs : 'a list) : ('a * 'a) list =
  zip [] def pred (List.filter pred xs)
and zip acc def pred xs =
  match xs with
  | []               -> List.rev acc
  | [last]           -> zip ((last, def) :: acc) def pred []
  | x1 :: x2 :: tail -> zip ((x1, x2) :: acc) def pred tail


(*
  Task 3:

  Write a function
  filterAndCreatePairsFold : 'a -> ('a -> bool) -> 'a list -> ('a * 'a) list
  that behaves similarly to the function defined in Task 2.
  Make sure your implementation uses List.fold or List.foldBack or its multiple
  argument conterparts appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)

let filterAndCreatePairsFold (def : 'a) (pred : 'a -> bool) (xs : 'a list) : ('a * 'a) list =
  (*
    1. Filter `xs` list
    1. Divide filtered list on two lists - odd and even
    2. Align the lists to have the same length, adding `def` element to a smaller one
    3. Fold the lists on `List.fold2`, accumulating tuples from both lists
  *)

  let split (odd, even, i) x =
    match i % 2 <> 0 with
      | true  -> (odd, x :: even, i + 1)
      | false -> (x :: odd, even, i + 1)

  let augment odd even =
    match (List.length odd), (List.length even) with
    | m, n when m > n -> (odd, def :: even)
    | m, n when m < n -> (def :: odd, even)
    | _               -> (odd, even)

  let odd, even, _       = List.fold split ([], [], 0) (List.filter pred xs)
  let augOdd, augEven    = augment odd even
  let zipFold2 acc x1 x2 = (x1, x2) :: acc

  List.fold2 zipFold2 [] augOdd augEven


(*
  Task 4:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function minAndMaxInTree : int Tree -> int * int that returns the the min and max elements
  of the tree.
  Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Tip   of 'a
  | Node of 'a Tree * 'a Tree

let rec minAndMaxInTree (tree : int Tree) : int * int =
  maxt tree id
and maxt t c =
  match t with
  | Tip a         -> c (a, a)
  | Node (tl, tr) ->
    maxt tl (
      fun (tlmin, tlmax) -> maxt tr (fun (trmin, trmax) ->
          match tlmin < trmin, tlmax > trmax with
          | true,  true  -> c (tlmin, tlmax)
          | true,  false -> c (tlmin, trmax)
          | false, true  -> c (trmin, tlmax)
          | false, false -> c (trmin, trmax)
        ))

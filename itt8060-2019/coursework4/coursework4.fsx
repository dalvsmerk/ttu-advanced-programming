(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Working with HTML trees

  ------------------------------------
  Name: Vladyslav Umerenko
  Tallinn University of Technology Student ID
  or Uni-ID: vlumer
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2019 under your name, into a file coursework4/coursework4.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// The following type declarations are given:

type Kind = Div | Par | Hdg

type Class = string

type Identifier = string


// Task 0
// Define a data type HTree for representing an html tree. The data type needs to support Div, Par and Hdg
// elements, style classes and optional identifiers. Content of each element can contain either 0 or more elements.
// Please note that you will use the type definition in futher tasks, but task 0 will not be tested on its own.

type HTree = Element list
and Element =
    | Node of Kind * Identifier option * Class list * HTree
    | Content of string


// Task 1
// Define a function
// makeElement : Kind -> Identifier option -> HTree
// that can be used to create HTree elements given kind and optional identifier.
// Examples:
// makeElement Div None
// makeElement Hdg (Some "id1")

let makeElement (kind : Kind) (id : Identifier option) : HTree =
  [Node (kind, id, [], [])]


// Task 2
// Define a function
// addClass : Class -> HTree -> HTree
// Such that the function could be used to add a class to an HTree element.
//
// and a function
// addContent : string -> HTree -> HTree
// that will add appropriate string content to the particular element
//
// and a function
// addChild : HTree -> HTree -> HTree
// that could be used to add child to a HTree. The first argument is the child,
// the second element is the one to which the child gets added to.

let addClass (treeClass : string) (tree : HTree) : HTree =
  match tree with
  | [] -> []
  | Node (kind, id, classes, children) :: t ->
    (Node (kind, id, classes @ [treeClass], children)) :: t
  | Content c :: t -> (Content c) :: t

let addContent (content : string) (tree : HTree) : HTree =
  match tree with
  | [] -> [Content content]
  | Node (kind, id, classes, []) :: t -> Node (kind, id, classes, [Content content]) :: t
  | Node (kind, id, classes, children) :: t ->
    (Node (kind, id, classes, children @ [Content content])) :: t
  | Content c :: t -> Content c :: t

let addChild (child : HTree) (tree : HTree) : HTree =
  match tree with
  | [] -> child
  | Node (kind, id, classes, children) :: t -> Node (kind, id, classes, children @ child) :: t
  | Content c :: t -> Content c :: t


// Task 3
// Define the function
// countElems : HTree -> int
// that will count the number of all elements in the tree.

let rec countElems (tree : HTree) : int =
  match tree with
  | [] -> 0
  | Node (_, _, _, children) :: t -> 1 + countElems children + countElems t
  | Content _ :: t -> countElems t


// Task 4
// Define a function of type
// listElems : HTree -> Kind list list
// that will return a full path of each element in the tree.
//
// And a function
// showContent : HTree -> string
// That will concatenate all string content in the tree following
// in order traversal pattern.

let rec listElems (tree : HTree) : Kind list list =
  let rec paths (parent : Kind list) (tree : HTree) : Kind list list =
    match tree with
    | [] -> [parent]
    | Node (kind, _, _, children) :: t -> (paths (parent @ [kind]) children) @ paths parent t
    | Content _ :: t -> paths parent t

  let removeEmpty lst = not (List.isEmpty lst)
  let preprocess lst = List.filter removeEmpty (List.rev lst)

  match tree with
  | [] -> []
  | Node (kind, _, _, []) :: t       -> preprocess ([[kind]] @ paths [] t)
  | Node (kind, _, _, children) :: t -> preprocess ((paths [kind] children) @ paths [] t)
  | Content _ :: t                   -> preprocess (paths [] t)

let rec showContent (tree : HTree) : string =
  match tree with
  | [] -> ""
  | Content c :: t -> c + showContent t
  | Node (_, _, _, children) :: t -> showContent children + showContent t


// Given the following type declarations

type Select = ByKind   of Kind
            | ById     of Identifier
            | ByClass  of Class

type Selector = Any
              | Select   of Select
              | Sequence of Selector * Selector


// The instances of the types can be used to
// define queries, e.g.
// the parent-child relation of x y : Select would be:
// Sequence ( ZeroOrMore Any, Sequence (Select x, Select y))
//
// and descendant relation of x y : Select would be:
// Sequence ( ZeroOrMore Any, Sequence (Select x, Sequence (ZeroOrMore Any, Select y)))


// Task 6
// Like previous, but should find all elements satisfying the selector
// select : Selector -> HTree -> (Kind list * HTree) list

let rec select (query : Selector) (tree : HTree) : (Kind list * HTree) list =
  selectByQuery [] query tree
and selectByQuery (parentPath : Kind list) (query : Selector) (parents : HTree) =
  match parents with
  | Node (kind, id, classList, _) as currentNode :: tail ->
    match query with
    | Any       -> (parentPath, [currentNode]) :: selectByQuery parentPath query tail
    | Select by ->
      match by with
      | ByKind queryKind ->
        if   queryKind = kind
        then (parentPath, [currentNode]) :: selectByQuery parentPath query tail
        else selectByQuery parentPath query tail

      | ById queryId ->
        match id with
        | Some ii when System.String.Equals(queryId, ii) ->
          (parentPath, [currentNode]) :: selectByQuery parentPath query tail
        | _ -> selectByQuery parentPath query tail

      | ByClass queryClass when List.contains queryClass classList ->
        (parentPath, [currentNode]) :: selectByQuery parentPath query tail

      | _ -> selectByQuery parentPath query tail

    | Sequence (parentQuery, childrenQuery) ->
      List.fold
        (fun acc parent ->
          let (pathOfParent, parentNode) = parent

          match parentNode with
          | Node (parentKind, _, _, children) :: _ ->
            acc @ selectByQuery (pathOfParent @ [parentKind]) childrenQuery children
          | _ -> acc)
        []
        (selectByQuery parentPath parentQuery (currentNode :: tail))

  | Content _ :: tail -> selectByQuery parentPath query tail
  | [] -> []


// Task 5
// Define a function that finds the first element that satisfies the selector property (if such an element exists).
// It should be the first element in the order in which the children were added to the tree.
// (Kind list * HTree) will be the path to the element and the appropriate element.
//
// first : Selector -> HTree -> (Kind list * HTree) option

let first (query : Selector) (tree : HTree) : (Kind list * HTree) option =
  let found = select query tree
  match found with
  | []      -> None
  | hd :: _ -> Some hd



let rec mapTree (ps : (Kind list * HTree) list) (fn : HTree -> HTree) (tree : HTree) =
  let paths = ps |> List.map (fun (path, node) ->
    match node with
    | Node (kind, _, _, _) :: _ -> path @ [kind]
    | _ -> path
  )

  List.fold (mapTraverse fn) tree paths
and mapTraverse (fn : HTree -> HTree) (tree : HTree) (path : Kind list) =
  match tree with
    | Node (kind, i, c, children) as node :: tail ->
      match path with
      | [pathKind] when pathKind = kind -> (fn [node]) @ mapTraverse fn tail path
      | pathKind :: pathTail when pathKind = kind ->
        Node (kind, i, c, (mapTraverse fn children pathTail)) :: mapTraverse fn tail path
      | _ -> node :: mapTraverse fn tail path

    | Content c :: tail -> Content c :: mapTraverse fn tail path
    | [] -> []


// Task 7
// Define a function that adds a class to the elements that satisfy the selector
// addClass' : Selector -> Class -> HTree -> HTree

let rec addClass' (query : Selector) (cc : Class) (tree : HTree) : HTree =
  mapTree (select query tree) (fun t -> addClass cc t) tree


// Similarly, define a function that adds a child to a tree satisfying a selector.
// The first HTree is the element to be added and the second HTree is the tree to which to add.
// addChild' : Selector -> HTree -> HTree -> HTree

let rec addChild' (query : Selector) (child : HTree) (tree : HTree) : HTree =
  mapTree (select query tree) (fun t -> addChild child t) tree


// Task 8
//
// Define a function that deletes all elements satisfying the selector.
// Do not delete the root element.
// delete : Selector -> HTree -> HTree

let rec delete (query : Selector) (tree : HTree) : HTree =
  let paths = select query tree

  match paths, tree with
  | [([], _)], _ -> tree // root node
  | _ -> mapTree (select query tree) (fun _ -> []) tree

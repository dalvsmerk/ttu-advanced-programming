// Q2: Partial fun.

// a)
let apply f a =
    match f with
    | Some f ->
        match a with
        | Some a -> Some (f a)
        | None -> None
    | None -> None

apply (Some id) (Some 3) // Some 3
apply (Some ((+) 1)) None // None
apply (Some List.head) (Some [1; 2; 3]) // Some 1

// b)
let rec sequence xs =
    match xs with
    | [] -> Some []
    | head :: tail ->
        match head with
        | None -> None
        | Some c ->
            let res = sequence tail
            match res with
            | None -> None
            | Some res -> Some (c :: res)

sequence [Some 1; Some 2]
sequence [Some 1; None; Some 2]

// c)
let rec sequence' xs =
    transform [] xs
and transform acc xs =
    match xs with
    | [] -> Some (acc |> List.rev)
    | head :: tail ->
        match head with
        | None -> None
        | Some c -> transform (c :: acc) tail

sequence' [Some 1; Some 2]
sequence' [Some 1; None; Some 2]


// Q3: Expr. trees

type VName = string

type Expr = Var of VName
          | Neg of Expr
          | And of (Expr * Expr)
          | Or  of (Expr * Expr)

type Assignment = (VName * bool) list

let lookup (v : VName) (vs : Assignment) = true

// a)
let rec interpret e vs =
    match e with
    | Var v -> lookup v vs
    | Neg e1 -> not (interpret e1 vs)
    | And (e1, e2) -> (interpret e1 vs) && (interpret e2 vs)
    | Or (e1, e2) -> (interpret e1 vs) || (interpret e2 vs)

// b)
let rec variables e =
    match e with
    | Var v -> [v]
    | Neg e1 -> variables e1
    | And (e1, e2) ->
        (variables e1) @ (variables e2)
        |> List.groupBy id
        |> List.map (fun (v, _) -> v)
    | Or (e1, e2) ->
        (variables e1) @ (variables e2)
        |> List.groupBy id
        |> List.map (fun (v, _) -> v)
    // | And (e1, e2) -> (variables e1) @ (variables e2) |> List.distinct
    // | Or (e1, e2) -> (variables e1) @ (variables e2) |> List.distinct

// c)
type MathExpr =
    | Const of int
    | Sum of MathExpr * MathExpr
    | Diff of MathExpr * MathExpr
    | Prod of MathExpr * MathExpr

let rec eval e =
    match e with
    | Const c -> c
    | Sum (e1, e2) -> (eval e1) + (eval e2)
    | Diff (e1, e2) -> (eval e1) - (eval e2)
    | Prod (e1, e2) -> (eval e1) * (eval e2)

// d)
let rec commute e =
    match e with
    | Const c -> Const c
    | Diff (e1, e2) -> Diff ((commute e1), (commute e2)) // don't swap
    | Sum (e1, e2) -> Sum ((commute e2), (commute e1))
    | Prod (e1, e2) -> Prod ((commute e2), (commute e1))

// e)
let rec pn e =
    match e with
    | Const c -> string c
    | Sum (e1, e2) -> sprintf "+ %s %s" (pn e1) (pn e2)
    | Diff (e1, e2) -> sprintf "- %s %s" (pn e1) (pn e2)
    | Prod (e1, e2) -> sprintf "* %s %s" (pn e1) (pn e2)


// Q4: Euler method

let eulerStep f (h : float) t y =
    ((t + h), (y + h * (f t y)))

let newton t y = -0.05 * (y - 22.0)

let newtonNext1 p =
    let (t0, y0) = p
    eulerStep newton 1.0 t0 y0

// a)
let rec euler p =
    seq {
        yield p
        yield! euler (newtonNext1 p)
    }

// b)
let euler' p =
    Seq.unfold (fun p -> Some (p, newtonNext1 p)) p

// c)
let rec coolingAprox eps ps =
    match Seq.head ps with
    | _, y when y <= 22.0 + eps -> []
    | p                         -> p :: coolingAprox eps (Seq.tail ps)
    // | t, y -> if y <= 22.0 + eps
    //           then [] // end sequence
    //           else (t, y) :: coolingAprox eps (Seq.tail ps)

// d)
let rec coolingAprox' eps ps =
    aprox [] eps ps
and aprox acc eps ps =
    match Seq.head ps with
    | _, y when y <= 22.0 + eps -> acc |> List.rev
    | p -> aprox (p :: acc) eps (Seq.tail ps)


// Tests

// g)
let rec c b =
    match b with
    | [] -> []
    | d :: e -> (c [(List.head e)])

// j)
let rec f x =
    seq {
        yield x
        yield! f x
    }

// l)
List.collect (fun x -> [x + x]) [2; 3; 5]

// m)
let rec g x y =
    match y with
    | [] -> 1
    | h :: t -> x h + g x t

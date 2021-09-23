(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

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
  repository itt8060-2019 under your name, into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/
// Please note that you need not read the papers, just pick 7 papers that sound interesting to you from the database.
let bibliographyData : BibliographyItem list = [
  (["Solanki, Vinesh"; "Sustretov, Dmitry"], "The quantum harmonic oscillator", (1149, 1168), 2014);
  (["Hidary, Jack"], "Quantum Computing: An Applied Approach", (1, 351), 2019);
  (["Ying, Mingsheng"; "Feng, Yuan"], "Quantum loop programs", (221, 250), 2010);
  (["Feng, Yuan"; "Howard Dale"], "Quantum coins and quantum sampling", (1, 40), 2016);
  (["Bacon, Dave"; "van Dam, Wim"; "Amento, Brittanney"], "Recent progress in quantum algorithms", (84, 93), 2010);
  (["Portugal, Renato"; "Lavor, Carlile"], "A Primer on Quantum Computing", (1, 105), 2019);
  (["Amento, Brittanney"; "Solanki, Vinesh"], "Quantum Circuits for Cryptanalysis", (1, 345), 2016)
]

// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defind using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!
open System

let rec compareLists (x1 : string list) (x2 : string list) : int =
  match x1, x2 with
  | [], [] ->  0
  | [], _  -> -1
  | _, []  ->  1
  | h1 :: t1, h2 :: t2 ->
    let c = String.Compare (h1, h2, StringComparison.CurrentCultureIgnoreCase)
    if c = 0 then compareLists t1 t2
    else c

// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.
let compareAuthors (xx : BibliographyItem) (yy : BibliographyItem) : int =
  let (xxAuthors, _, _, _) = xx
  let (yyAuthors, _, _, _) = yy

  compareLists xxAuthors yyAuthors

// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are
// the same then according to years.
let compareAuthorsYears (xx: BibliographyItem) (yy: BibliographyItem) : int =
  let comparedByAuthors = compareAuthors xx yy

  match comparedByAuthors with
  | 0 ->
    let (_, _, _, xxYear) = xx
    let (_, _, _, yyYear) = yy
    xxYear - yyYear
  | _ -> comparedByAuthors

// 6. Make a function
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order
let rec sortBibliographyByYear (items : BibliographyItem list) =
  let compareByYear (xx : BibliographyItem) (yy : BibliographyItem) =
    let (_, _, _, xxYear) = xx
    let (_, _, _, yyYear) = yy
    xxYear - yyYear

  List.sortWith compareByYear items

// 7. Make a function
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order
let sortBibliographyByAuthorYear (items : BibliographyItem list) : BibliographyItem list =
  List.sortWith compareAuthorsYears items

// 8. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.
let rec groupByAuthor (items : BibliographyItem list) : (string * BibliographyItem list) list =
  let rec extractAuthorsFrom (source : BibliographyItem list) =
    match source with
    | [] -> []
    | (paperAuthors, _, _, _) :: tail -> paperAuthors @ extractAuthorsFrom tail

  let rec authorsContain (author : string) (paperAuthors : string list) : bool =
     match paperAuthors with
     | [] -> false
     | head :: tail -> String.Compare(author, head, StringComparison.CurrentCultureIgnoreCase) = 0 || authorsContain author tail

  let rec unique (authors : string list) : string list =
    match authors with
    | [] -> []
    | head :: tail when (authorsContain head tail) -> unique tail
    | head :: tail -> head :: unique tail

  let authors = unique (extractAuthorsFrom items)

  (*
    Two functions
    1. Find all papers for a separate author
    2. Use (1) to traverse all authors and combine (string * BibliographyItem list) to list
  *)

  let rec filterBy (author : string) (papers : BibliographyItem list) : BibliographyItem list =
    match papers with
    | [] -> []
    | paper :: tail ->
      let (authors, _, _, _) = paper
      if authorsContain author authors then paper :: filterBy author tail
      else filterBy author tail

  let rec groupBy (authors : string list) (source : BibliographyItem list) : (string * BibliographyItem list) list =
    match authors with
    | [] -> []
    | author :: tail -> (author, filterBy author source) :: groupBy tail source

  groupBy authors items

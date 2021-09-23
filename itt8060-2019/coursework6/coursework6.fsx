(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 6: property-based testing

  ------------------------------------
  Name: Vladyslav Umerenko
  Student ID: vlumer
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2019 under your name, into a file
  coursework6/coursework6.fsx by Nov 17.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.
*)

#if INTERACTIVE
#r    "FsCheck.dll"
#load "FileSystem.fs"
#endif

open FsCheck
open FileSystem


(*
   Question 1

   Define predicates

   fsTreeWf : FsTree -> bool

   pathWf   : string list -> bool

   that check whether the given tree is well-formed as a filesystem
   and whether the path (represented as a list of strings) is well-formed.

   A well-formed filesystem cannot have identical paths leading to
   different nodes in the tree.

   A well-formed path cannot contain nodes with empty names.

   FsTree is a tree data type that we think of as a filesystem.
   It is peculiar since there is no difference between files and direcotries,
   everything is a node.
*)

let pathWf p = not (List.contains "" p) && not (List.contains null p)

let fsTreeWf fs =
   let paths       = FileSystem.show fs
   let uniquePaths = List.distinct paths
   let pathsWf     = not (List.exists (pathWf >> not) paths)

   ((List.length paths) = (List.length uniquePaths)) && pathsWf


(*
   Question 2

   Define a FsCheck property

   createDirWf : string list -> FsTree -> Property

   which checks that creating a well-formed path p (using createDir)
   in a well-formed tree fs (filesystem) results in a well-formed tree.

   Define this using a conditional property (==>).

   Convince yourself that this is a bad way to do testing by observing
   the amount of test inputs that trivially satisfy this property.
*)

let createDirWf p fs =
   (pathWf p) ==> ((fsTreeWf fs) ==> (fsTreeWf (FileSystem.createDir p fs)))


(*
   Question 3

   Define a generator

   wfTree : Gen<FsTree>

   that generates only well-formed trees (filesystems).


   Define a generator

   wfPath : Gen<string list>

   that generates only well-formed filesystem paths.


   You may want to use the predicates defined above to check that
   the generated data indeed is well-formed.
*)

let wfPath : Gen<string list> = Arb.generate<string list> |> Gen.where pathWf

let genPath = Arb.generate<string> |> Gen.where (fun x -> x <> "")

let wfTree : Gen<FsTree> =
   let mapNodes path children = (path, children)

   let rec genNodes size =
      match size with
      | n when n > 0 -> Gen.map2 mapNodes genPath (genRoot (n - 1)) |> Gen.listOf
      | _            -> Gen.constant []
   and genRoot n =
      Gen.map Node (genNodes n)

   genRoot (System.Random().Next(0, 100)) |> Gen.where fsTreeWf


(*
   Question 4

   Define an FsCheck property

   deleteIsWellFormed : string list -> FsTree -> bool

   which checks that given
   p  : string list
   fs : FsTree
   we have that after deleting p from fs the result is well-formed.

   You may assume here that this property is only used with the
   "well-formed" generators.

   The correct behaviour of delete is that if p is not present in fs
   then fs is returned as is.
*)

let deleteIsWellFormed p fs =
   let paths           = FileSystem.show fs
   let pathExists      = List.exists (fun x -> x = p) paths

   let fs'    = FileSystem.delete p fs
   let treeWf = fsTreeWf fs'

   match fs', fs with
   | Node f', Node f ->
      match pathExists with
      | true  -> treeWf && (f' <> f)
      | false -> treeWf && (f' = f)

(*
   Question 5

   Define an FsCheck property

   createDirExists : string list -> FsTree -> bool

   which checks that given
   p  : string list
   fs : FsTree
   we have that the path p is included (exactly once) in the
   result of show after we have created the directory p in fs.

   Here you may assume that this property is used only
   with well-formed generators, i.e., the fs and p that
   are passed in as parameters satisfy the well-formedness condition.

   The correct behaviour of createDir p fs is that it returns
   the given fs if p already exists (as a directory) in fs.
*)

let createDirExists p fs =
   let counter acc path = if path = p then acc + 1 else acc

   let fs'    = FileSystem.createDir p fs
   let paths' = FileSystem.show fs'

   let occurance       = List.fold counter 0 paths'
   let uniqueOccurance = 1 = occurance

   match fs, fs' with
   | Node n, Node n' -> uniqueOccurance && (n = n')


(*
   Question 6

   Define an FsCheck property

   deleteDeletes : FsTree -> bool

   which checks that given an
   fs : FsTree
   we have that by deleting one by one all of the items in the result of
   show fs we end up with an empty filesystem.

*)

let deleteDeletes fs =
   let deleter acc path = FileSystem.delete path acc
   let paths            = FileSystem.show fs

   match List.fold deleter fs paths with
   | Node [] -> true
   | Node _  -> false



(*
   Question 7

   Define an FsCheck property

   createAndDelete : FsTree -> string list -> string list -> Property

   which checks that given

   fs : FsTree
   p1 : string list
   p2 : string list

   we have that if p1 is not a prefix of p2 then

   1) creating directory p1 in fs
   2) creating directory p2 in the result
   3) deleting p1 from the result

   gives a filesystem which still contains p2.

   fs, p1, p2 are again from "well-formed" generators.
*)

let createAndDelete fs p1 p2 =
   let fs'   = FileSystem.createDir p1 fs
   let fs''  = FileSystem.createDir p2 fs'
   let fs''' = FileSystem.delete p1 fs''

   List.contains p2 (FileSystem.show fs''')

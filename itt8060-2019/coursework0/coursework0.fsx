(*
  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 0: Getting started
  ------------------------------------
  Name: Vladyslav Umerenko
  Student ID:
  ------------------------------------
  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VS/Mono on your laptop, etc.


// 1. Load the  following function into fsi
let greeting name = printfn "Hello: %s" name

// 2. Run the function greeting and  say hello to yourself.
greeting "Vladyslav"

// 3. Create a value myName : string that contains your name.
let myName : string = "Vladyslav"

// 4.Define
// splitAtChar : text:string -> sep:char -> list<string>
let splitAtChar (text: string) =
  let fromChar (sep: char) =
    let words = text.Split sep |> Array.toList
    words
  fromChar

let charList = splitAtChar "tere" 'r' // ["te"; "e"]

// 5. Write a function splitAtSpaces in such a way that it uses splitAtChar
// Hint: we defined splitAtSpaces in the lecture, now you need to modify it.
let splitAtSpaces (text: string) =
  splitAtChar text ' '

// 6. Define sentenceCount : text:string -> int
let sentenceCount (text: string) =
  let filterNonEmptyString (text: string) =
    text.Length > 0

  let mapSentenceToLength (sentence: string) =
    sentence.Length

  let sumSentenceLength (acc: int) =
    let sum (length: int) =
      length + acc
    sum

  let trimSentence (sentence: string) =
    sentence.Trim()

  let sentencesWithEmpty = splitAtChar text '.'
  let sentencesTrimmed = List.map trimSentence sentencesWithEmpty
  let sentences = List.filter filterNonEmptyString sentencesTrimmed

  let amountOfSentences = sentences.Length
  let sentencesLength = List.map mapSentenceToLength sentences
  let sentencesLengthSum = List.fold sumSentenceLength 0 sentencesLength
  let averageSentenceLength = sentencesLengthSum / amountOfSentences

  amountOfSentences, averageSentenceLength

// 7. Define stats : text:string -> unit
// which prints the same stats as showWordCount +
// the number of sentences and average length of sentences
// hint: try float: int -> float
let wordCount (text: string) =
  let words = splitAtSpaces text
  let wordsSet = Set.ofList words
  let amountOfWords = words.Length
  let amountOfDublicates = words.Length - wordsSet.Count

  amountOfWords, amountOfDublicates

let wordStatistics (text: string) =
  let amountOfWords, amountOfDublicates = wordCount text
  let amountOfSentences, averageSentenceLength = sentenceCount text

  amountOfWords, amountOfDublicates, amountOfSentences, averageSentenceLength

let stats (text: string) =
  let textStats = wordStatistics text

  match textStats with
  | (a, b, c, d) -> printf "n_words=%d\nn_dubs=%d\nn_sent=%d\navg_sent_len=%d\n" a b c d

// 8. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally
open System.IO
open System.Net

let makeHttpRequest (url: string) =
  let request = WebRequest.Create url

  let response = request.GetResponse()
  let responseStream = response.GetResponseStream()

  let reader = new StreamReader(responseStream)
  let responseBody = reader.ReadToEnd()
  response.Close()

  responseBody

let documentURL = "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"
let document = makeHttpRequest documentURL

// 9. run stats on the downloaded file
// stats document
stats document

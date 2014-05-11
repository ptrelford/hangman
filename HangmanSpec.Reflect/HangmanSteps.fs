module HangmanSteps

type HangmanFixture () = inherit TickSpec.NUnit.FeatureFixture("Hangman.feature")

open Hangman
open TickSpec
open NUnit.Framework

let mutable word = ""
let mutable guesses : char list = []

let [<BeforeScenario>] Setup () =
    word <- ""
    guesses <- []

let [<Given>] ``the word '(.*)'`` (s:string) =
    word <- s
let [<Given>] ``no guesses`` () =
    guesses <- []

let [<When>] ``the letter '(.*)' is guessed`` (guess:char) =
   guesses <- guess::guesses

let [<Then>] ``the display word is '(.*)'`` (expected:string) =
   let actual = toPartialWord word guesses
   Assert.AreEqual(expected, actual)

let [<Then>] ``the tally is (.*)`` (expected:int) =
   let actual = Hangman.tally word guesses
   Assert.AreEqual(expected, actual)

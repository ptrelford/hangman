module Hangman.Tests

open TickSpec
open TickSpec.Fold
open NUnit.Framework
open Hangman

let hangmanFeature = """
Feature: Play Hangman

Background:
 Given the word 'HANGMAN'
 And no guesses

Scenario: Letter 'A' occurs
 When the letter 'A' is guessed
 Then the display word is '_A___A_'

Scenario: Letter 'B' does not occur
 When the letter 'B' is guessed
 Then the tally is 1

Scenario: Letters 'BC' do not occur
 When the letter 'B' is guessed
 And  the letter 'C' is guessed
 Then the tally is 2

Scenario: Letters 'ABH' guessed
 When the letter 'A' is guessed
 And  the letter 'B' is guessed
 And  the letter 'H' is guessed
 Then the tally is 1
 Then the display word is 'HA___A_'
"""
let hangmanLines = hangmanFeature.Split([|'\r';'\n'|])

let performStep (word,guesses) step =
   match step with
   | Given "the word '(.*)'" [word] ->     
      word,guesses
   | Given "no guesses" [] ->
      word,[]
   | When "the letter '(.*)' is guessed" [Char letter] ->
      word, letter::guesses   
   | Then "the display word is '(.*)'" [expected] ->
      let actual = toPartialWord word guesses
      Assert.AreEqual(expected, actual)
      word, guesses
   | Then "the tally is (.*)" [Int expected] ->
      let actual = Hangman.tally word guesses
      Assert.AreEqual(expected, actual)
      word, guesses
   | _ -> notImplemented ()

runFeature hangmanLines performStep ("",[])

type HangmanFixture() = 
   inherit FeatureFixture<State>(hangmanLines,performStep,("",[]))
and State = string * char list
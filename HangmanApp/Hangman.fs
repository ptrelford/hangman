module Hangman

let hangman = [ """ 
 ____
|/   |
|   
|    
|    
|    
|
|_____
""";
"""
 ____
|/   |
|   (_)
|    
|    
|    
|
|_____
""";
"""
 ____
|/   |
|   (_)
|    |
|    |    
|    
|
|_____
""";
"""
 ____
|/   |
|   (_)
|   \|
|    |
|    
|
|_____
""";
"""
 ____
|/   |
|   (_)
|   \|/
|    |
|    
|
|_____
""";
"""
 ____
|/   |
|   (_)
|   \|/
|    |
|   / 
|
|_____
""";
"""
 ____
|/   |
|   (_)
|   \|/
|    |
|   / \
|
|_____
""";
"""
 ____
|/   |
|   (_)
|   /|\
|    |
|   / \
|
|_____
"""]


open System

let tally (word:string) guesses =
   guesses |> Seq.filter (fun c ->
      not (String.exists ((=) c) word)
   ) |> Seq.length

let toPartialWord (word:string) (guesses:char seq) =
   word |> String.map (fun c -> 
      if Seq.exists ((=) c) guesses then c else '_'
   )

let isGuessValid (guesses:char seq) (guess:char) =
   Seq.exists ((=) guess) ['A'..'Z'] &&
   not (guesses |> Seq.exists ((=) guess))

let rec readGuess guesses =
   let guess = Console.ReadKey(true).KeyChar |> Char.ToUpper
   if isGuessValid guesses guess then guess
   else readGuess guesses

let queryGuess guesses =
   Console.Write("Guess: ")
   let guess = readGuess guesses
   Console.WriteLine(guess)
   guess

let rec play word guesses =
   let tally = tally word guesses
   Console.Write(hangman.[tally])
   let word' = toPartialWord word guesses
   Console.WriteLine(word')
   if word = word' then 
      Console.WriteLine("CORRECT")
   elif tally = hangman.Length-1 then 
      Console.WriteLine("HANGMAN")
   else
      let guess = queryGuess guesses
      play word (guess::guesses)
 
let words = ["PENCIL";"CHALK";"CRAYON";"BRUSH"]

let word = words.[Random().Next(words.Length)]
do play word []
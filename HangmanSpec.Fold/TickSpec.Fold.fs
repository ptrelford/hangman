module TickSpec.Fold

open TickSpec
    
[<AutoOpen>]
module Exceptions =
    exception Pending of unit
    let pending () = raise <| Pending()
    let notImplemented () = raise <| new System.NotImplementedException()

[<AutoOpen>]
module Patterns =
    open System.Text.RegularExpressions
    let private Regex input pattern =
        let r = Regex.Match(input,pattern)
        if r.Success then Some [for i = 1 to r.Groups.Count-1 do yield r.Groups.[i].Value]
        else None
    let (|Given|_|) (pattern:string) (step) =
        match step with
        | GivenStep input -> Regex input pattern        
        | WhenStep _ | ThenStep _ -> None
    let (|When|_|) (pattern:string) (step) =
        match step with
        | WhenStep input -> Regex input pattern        
        | GivenStep _ | ThenStep _ -> None    
    let (|Then|_|) (pattern:string) (step) =
        match step with
        | ThenStep input -> Regex input pattern        
        | GivenStep _ | WhenStep _ -> None
    let (|Char|) (text:string) = text.[0]
    let (|Int|) s = System.Int32.Parse(s)

[<AutoOpen>]
module Runner =
    open System

    let print color text =
        let old = Console.ForegroundColor
        Console.ForegroundColor <- color
        printfn "%s" text
        Console.ForegroundColor <- old

    let tryStep performStep state (step,line) =
        try 
            let acc = performStep state step
            print ConsoleColor.Green line.Text
            acc
        with e ->
            print ConsoleColor.Red line.Text
            printfn "Line %d: %A" line.Number e
            reraise ()

    let runFeature (feature:string[]) performStep init =
        let feature = feature |> FeatureParser.parseFeature        
        feature.Scenarios
        |> Seq.filter (fun scenario -> scenario.Tags |> Seq.exists ((=) "ignore") |> not) 
        |> Seq.iter (fun scenario ->
            print ConsoleColor.Green scenario.Name            
            scenario.Steps |> Array.scan (tryStep performStep) init
            |> ignore
        )


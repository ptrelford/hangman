[<AutoOpen>]
module TickSpec.NUnit

open TickSpec
open NUnit.Framework

[<TestFixture>]
type FeatureFixture<'TState>
      (featureLines:string[], 
       doStep:'TState->StepType->'TState, 
       initState:'TState) =
    [<Test>]
    [<TestCaseSource("Scenarios")>]
    member this.TestScenario (scenario:ScenarioSource) =
        scenario.Steps 
        |> Seq.scan (fun acc (step,line) -> doStep acc step) initState
        |> ignore
    member this.Scenarios =       
        let feature = FeatureParser.parseFeature featureLines
        feature.Scenarios
        |> Seq.filter (fun scenario ->
            scenario.Tags |> Seq.exists ((=) "ignore") |> not
        )

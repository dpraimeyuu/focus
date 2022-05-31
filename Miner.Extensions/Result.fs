namespace Miner.Extensions

[<AutoOpen>]
module Result =
    let traverse (rs: Result<'a, 'b> array): Result<'a array, 'b> =
        rs
        |> List.ofArray
        |> List.fold (fun rs' r ->
            match rs', r with
            | Ok rs', Ok r ->
                Ok (r :: rs')
            | Error rs', _ -> Error rs'
            | _, Error r -> Error r
        ) (Ok List.empty)
        |> Result.map (Array.ofList)

    let inline (>>=) fn res = Result.bind fn res
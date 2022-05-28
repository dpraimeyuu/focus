module Domain =
    open System
    
    module GitLog =
        type CommitSha = private CommitSha of sha: string
        type CommitAuthor = private CommitAuthor of author: string
        type CommitDate = private CommitDate of date: DateTime
        type FilePath = private FilePath of string
        type [<Measure>] addedLines
        type [<Measure>] removedLines
        type GitChange = private {
            AddedLines: int<addedLines>
            RemovedLines: int<removedLines>
            FilePath: FilePath
        } with
            static member From(rawChange: string): Result<GitChange, string> =
                let rawChange = rawChange.Split("\r\t")
                match rawChange with
                | [| addedLines; removedLines; file|] ->
                    Ok {
                        AddedLines = 
                            addedLines
                            |> int
                            |> LanguagePrimitives.Int32WithMeasure;
                        RemovedLines =
                            removedLines
                            |> int
                            |> LanguagePrimitives.Int32WithMeasure;
                        FilePath = FilePath file
                    }
                | _ -> Error (sprintf "Error parsing git change %A for a single entry" rawChange)

        type GitLogEntry = private {
            CommitSha: CommitSha
            Date: CommitDate
            Author: CommitAuthor
            Changes: GitChange array
        }
        
        let private splitEntryByLine (singleRawEntry: string) =
            singleRawEntry.Split("\n")
        type private GitLogRawEntry = GitLogRawEntry of rawEntry: string array
        let private splitByEntry (gitLogContent: string): GitLogRawEntry array =
            gitLogContent.Split("\n\n")
            |> Array.map (splitEntryByLine >> GitLogRawEntry)

        type private GitLogEntryHeader = private {
            CommitSha: CommitSha
            Date: CommitDate
            Author: CommitAuthor
        } with
            static member From(rawHeader: string): Result<GitLogEntryHeader, string> =
                let parsedHeader = 
                    rawHeader.Replace("'", "").Split("--")
                    |> Array.filter (not << String.IsNullOrEmpty)

                match parsedHeader with
                | [| sha; date; author |] ->
                    try
                        Ok {
                        CommitSha = CommitSha sha;
                        Date = date |> DateTime.Parse |> CommitDate;
                        Author = CommitAuthor author
                    }
                    with
                        ex -> Error ($"Error while parsing a commit date of {rawHeader}.\nError message: {ex.Message}\nStack trace: {ex.StackTrace}")
                | _ -> Error $"Wrong format of git entry header. Please check your `--pretty=format` argument of `git log` command"
            
        let private parseGitLogChanges (rawGitChanges: string array) =
            rawGitChanges |> Array.map GitChange.From
            
        let private parseGitLogEntryHeader (rawHeader: string): Result<GitLogEntryHeader, string> =
            GitLogEntryHeader.From rawHeader

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
        let private parseEntry (rawEntry: GitLogRawEntry): Result<GitLogEntry, string> =
            match rawEntry with
            | GitLogRawEntry rawEntry' ->
                match rawEntry' |> List.ofArray with
                | headerLine :: rawGitChanges ->
                    let header = parseGitLogEntryHeader headerLine
                    printfn "%A" rawGitChanges
                    let gitChanges = 
                        rawGitChanges
                        |> Array.ofList
                        |> parseGitLogChanges
                        |> Result.traverse


                    match header, gitChanges with
                    | Ok header, Ok gitChanges ->
                        Ok {
                            CommitSha = header.CommitSha;
                            Author = header.Author;
                            Date = header.Date;
                            Changes = gitChanges
                        }
                    | Error header, _ -> Error header
                    | _, Error gitChanges -> Error gitChanges
                | _ -> Error $"Error while parsing a single entry {rawEntry}"

        let private parseEntries (rawEntries: GitLogRawEntry array) =
            rawEntries
            |> Array.map parseEntry
            |> Result.traverse
        let parse (gitLogContent: string): Result<GitLogEntry array, string> =
            gitLogContent
            |> splitByEntry
            |> parseEntries

open Domain

let logFile = System.IO.File.ReadAllText(@"./logfile.log")
let logs = GitLog.parse logFile
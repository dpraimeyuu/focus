#load "Result.fsx"
module Domain =
    open System
    open Result
    module GitLog =
        type CommitSha = private CommitSha of sha: string
        type CommitAuthor = private CommitAuthor of author: string
        type CommitDate = private CommitDate of date: DateTime
        type FilePath = private FilePath of string
        type [<Measure>] addedLines
        type [<Measure>] removedLines
        
        let private isNumeric (s: string): bool = 
            Int16.TryParse s
            |> fst
        type AddedLines =
        | NotApplicable
        | AddedLines of lines: int<addedLines>
        with
                static member From(linesNumber: string) =
                    if linesNumber |> isNumeric
                        then
                            linesNumber
                            |> int
                            |> LanguagePrimitives.Int32WithMeasure
                            |> AddedLines
                        else
                            NotApplicable
        type RemovedLines =
        | NotApplicable
        | RemovedLines of lines: int<removedLines>
            with
                static member From(linesNumber: string) =
                    if linesNumber |> isNumeric
                        then
                            linesNumber
                            |> int
                            |> LanguagePrimitives.Int32WithMeasure
                            |> RemovedLines
                        else
                            NotApplicable

        type GitChange = private {
            AddedLines: AddedLines
            RemovedLines: RemovedLines
            FilePath: FilePath
        } with
            static member From(rawChange: string): Result<GitChange, string> =
                printfn $"single change {rawChange}"
                let rawChange = rawChange.Split("\t")
                match rawChange with
                | [| addedLines; removedLines; file|] ->
                    Ok {
                        AddedLines = AddedLines.From addedLines
                        RemovedLines = RemovedLines.From removedLines
                        FilePath = FilePath file
                    }
                | _ -> Error ($"Error parsing git change for a single entry")

        type GitLogEntry = private {
            CommitSha: CommitSha
            Date: CommitDate
            Author: CommitAuthor
            Changes: GitChange array
        }
        
        type GitLogRawEntry = GitLogRawEntry of rawEntry: string array
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
                | _ -> Error $"Wrong format of git entry header. Please check your `--pretty=format` argument of `git log` command: Raw header: {rawHeader}"
            
        let private isEntryHeader (rawEntryLine: string) = rawEntryLine.StartsWith("'--")
        let private parseGitLogChanges = 
            (Array.skipWhile isEntryHeader) >> (Array.map GitChange.From)

        let private parseGitLogHeaders = 
            (Array.takeWhile isEntryHeader) >> (Array.map GitLogEntryHeader.From)

        let private mergeEntryHeaderWithGitChanges gitChanges headerLine =
            match headerLine, gitChanges with
            | Ok header, Ok gitChanges ->
                Ok {
                    CommitSha = header.CommitSha;
                    Author = header.Author;
                    Date = header.Date;
                    Changes = gitChanges
                }
            | Error header, _ -> Error header
            | _, Error gitChanges -> Error gitChanges
                    
        let private parseEntry (rawEntry: GitLogRawEntry): Result<GitLogEntry array, string> =
            match rawEntry with
            | GitLogRawEntry rawEntry' ->

                let headerLines' =
                    rawEntry' |> parseGitLogHeaders
                let gitChanges = 
                    rawEntry'
                    |> parseGitLogChanges
                    |> Result.traverse

                let gitLogEntries = 
                    headerLines'
                    |> Array.map (mergeEntryHeaderWithGitChanges gitChanges)
                    |> Result.traverse

                gitLogEntries

        let private parseEntries (rawEntries: GitLogRawEntry array) =
            rawEntries
            |> Array.map parseEntry
            |> Result.traverse
            |> Result.map (Array.collect id)

        [<Literal>]
        let private newLine = "\n"
        [<Literal>]
        let private emptyLine = newLine + newLine
        
        let private splitLinesWithinEntry (singleRawEntry: string) =
            singleRawEntry.Split(newLine)

        let splitByEntry (gitLogContent: string): GitLogRawEntry array =
            gitLogContent.Split(emptyLine)
            |> Array.map (splitLinesWithinEntry >> GitLogRawEntry)


        let parse (gitLogContent: string): Result<GitLogEntry array, string> =
            gitLogContent
            |> splitByEntry
            |> parseEntries

open Domain
open System

let logFile = System.IO.File.ReadAllText(@"./logfile.log", Text.Encoding.UTF8)
let logs = GitLog.parse logFile
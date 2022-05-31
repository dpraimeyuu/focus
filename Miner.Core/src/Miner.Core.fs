namespace Miner.Core

module Domain =
    open System
    open Miner.Extensions
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

        module Analysis =
            module Summary =
                type AuthorsCount = private AuthorsCount of authorsCount: int
                    with static member From(gitLogEntries: GitLogEntry array): AuthorsCount =
                            gitLogEntries
                            |> Array.distinctBy (fun e -> e.Author)
                            |> Array.length
                            |> AuthorsCount

                type CommitsCount = private CommitsCount of commitsCount: int
                with static member From(gitLogEntries: GitLogEntry array): CommitsCount =
                        gitLogEntries 
                        |> Array.length
                        |> CommitsCount

                type EntitiesCount = private EntitiesCount of entitiesCount: int
                with static member From(gitLogEntries: GitLogEntry array): EntitiesCount =
                        gitLogEntries
                        |> Array.collect (fun e -> e.Changes)
                        |> Array.distinctBy (fun change -> change.FilePath)
                        |> Array.length
                        |> EntitiesCount

                type ChangedEntitiesCount = private ChangedEntitiesCount of changedEntitiesCount: int
                with static member From(gitLogEntries: GitLogEntry array): ChangedEntitiesCount =
                            gitLogEntries
                            |> Array.collect (fun e -> e.Changes)
                            |> Array.length
                            |> ChangedEntitiesCount
                
                type Summary = private {
                    CommitsCount: CommitsCount
                    EntitiesCount: EntitiesCount
                    ChangedEntitiesCount: ChangedEntitiesCount
                    AuthorsCount: AuthorsCount
                }
                with static member From(gitLogEntries: GitLogEntry array): Summary =
                        {
                            AuthorsCount = AuthorsCount.From(gitLogEntries)
                            EntitiesCount = EntitiesCount.From(gitLogEntries)
                            ChangedEntitiesCount = ChangedEntitiesCount.From(gitLogEntries)
                            CommitsCount = CommitsCount.From(gitLogEntries)
                        }
            type [<Measure>] revision
            type TotalRevisions = private {
                File: FilePath
                Revisions: int<revision>
            } 
                with
                    static member From(file: FilePath, changes: GitChange array) =
                        {
                            File = file
                            Revisions =
                                changes
                                |> Array.length
                                |> LanguagePrimitives.Int32WithMeasure
                        }

        let summary = Analysis.Summary.Summary.From

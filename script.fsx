#I "Domain.fs"

open Domain
open System

let logFile = System.IO.File.ReadAllText(@"./logfile.log", Text.Encoding.UTF8)
let logs = GitLog.parse logFile

logs
|> Result.map GitLog.Analysis.summary
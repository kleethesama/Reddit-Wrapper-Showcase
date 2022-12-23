module Io

open System.IO

let createTextFile (path : string) (s : string) =
  task {
    return! File.WriteAllTextAsync(path, s)
  }

let readTextFile (path : string) =
  task {
    return! File.ReadAllTextAsync(path)
  }
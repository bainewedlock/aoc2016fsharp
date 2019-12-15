module Input

open System.IO


let asList name =
  [|__SOURCE_DIRECTORY__; "input"; name|]
  |> Path.Combine
  |> FileInfo
  |> fun f -> File.ReadAllLines(f.FullName)
  |> Array.toList



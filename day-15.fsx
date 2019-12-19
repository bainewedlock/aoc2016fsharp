open System
open System.Text.RegularExpressions

#load "input.fsx"

type Disc =
  {
    no              : int
    positions       : int
    initialPosition : int
  }

let pattern = @"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)."

let parseLine line =
  let m = Regex.Match(line, pattern)
  if m.Success
  then
    m.Groups
    |> Seq.cast<Group>
    |> Seq.tail
    |> Seq.map (fun x -> int x.Value)
    |> Seq.toList
    |> function 
    | [d;p;i] -> { no = d; positions = p; initialPosition = i }
    | err -> failwithf "%A" err
  else failwithf "bad line: %s" line

let discs =
  Input.asList "input-15.txt"
  |> List.map parseLine

let discsAt discs time =
  discs
  |> List.map (fun d ->
    let time' = time + d.no
    (time' + d.initialPosition) % d.positions)

let rec solve discs x =
  if discsAt discs x |> List.sum = 0
  then x
  else solve discs (x+1)

let answer = solve discs 0

// -----------------------------
// ---------- PART II ----------
// -----------------------------

let maxNo = List.map (fun d -> d.no) >> List.max

let append discs positions initialPosition =
  let d =
    { 
      no              = (maxNo discs) + 1
      positions       = positions
      initialPosition = initialPosition
    }
  discs @ [d]

let discs' = append discs 11 0

let answer' = solve discs' 0
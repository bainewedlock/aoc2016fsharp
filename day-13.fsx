open System

#load "input.fsx"

let f favoriteNumber (x, y) =
  x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
  |> fun d -> Convert.ToString(d, 2).Replace("0", "").Length % 2 = 1

let grow (x, y) =
  [(0, -1); (1,  0); (0,  1); (-1, 0)]
  |> List.choose (fun (dx, dy) ->
    match x+dx, y+dy with
    | x', y' when x'<0 || y'<0 -> None
    | x', y'                   -> Some (x', y'))

let rec floodFill fFree steps fStop spaces seen =
  let spaces' =
    spaces
    |> List.collect grow
    |> List.except seen
    |> List.filter fFree
  let seen' = seen |> Set.union(Set spaces')
  if fStop steps seen' then (Some steps, seen)
  else floodFill fFree (steps+1) fStop spaces' seen'

let favoriteNumber = 1362

let (answer, _) =
  let fStop _ seen = seen |> Set.contains (31, 39)
  floodFill (f favoriteNumber >> not) 1 fStop [(1, 1)] Set.empty

let answer' =
  let fStop steps _ = steps = (50 + 1) // off by one error somewhere
  floodFill (f favoriteNumber >> not) 1 fStop [(1, 1)] Set.empty
  |> snd
  |> Set.count


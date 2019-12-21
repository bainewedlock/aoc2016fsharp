module day_16

type Bit = byte
let fromChar (b: char) = (byte b) - 48uy
let fromBool (b: bool) = if b then 1uy else 0uy
let invert (b: Bit) = 1uy - b

let step (xs: Bit list) =
  xs @ [0uy] @ (xs |> List.rev |> List.map invert)

let solve input n =
  let rec loop bits =
    match List.length bits >= n with
    | true  -> bits
    | false -> loop (step bits)
  input
  |> Seq.map fromChar
  |> Seq.toList
  |> loop
  |> List.take n

let rec checksum bits =
  bits
  |> List.chunkBySize 2
  |> List.map (List.sum >> (<>) 1uy >> fromBool)
  |> function
  | bits' when List.length bits' % 2 = 1 -> bits'
  | bits' -> checksum bits'

let answer = 
  solve "01110110101001000" 272
  |> checksum
  |> List.map string
  |> List.reduce (+)

#time
let answer' = 
  solve "01110110101001000" 35651584
  |> checksum
  |> List.map string
  |> List.reduce (+)
#time


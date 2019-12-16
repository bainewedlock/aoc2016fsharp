open System.Security.Cryptography
open System.Text
open System

let enco = Encoding.ASCII
let md5 = MD5.Create()

[<Measure>] type Index
[<Measure>] type Byte
type HashFunc = int<Index> -> byte[]

type Repo = Map<int<Byte>, int<Index> Set>

type Cache = { ubound : int<Index>; n3s : Repo; n5s : Repo }
  with
    static member create =
      { ubound = -1<Index>; n3s = Map.empty; n5s = Map.empty }

let addSingle index byte (repo: Repo) =
  let v = defaultArg (repo.TryFind(byte)) Set.empty
  repo.Add(byte, v.Add(index))

let rec addMany index bytes (repo: Repo) =
  match bytes with
  | []     -> repo
  | x::xs  -> addMany index xs (addSingle index x repo)

let singleMd5 (salt: string) (x: int<Index>) = 
  sprintf "%s%d" salt x
  |> enco.GetBytes
  |> md5.ComputeHash

let rec grow (fHash: HashFunc) max cache =
  match cache.ubound + 1<Index> with
  | x when x > max -> cache
  | x ->
  let (_,_,n3,n5) =
    x
    |> fHash
    |> Array.toSeq
    |> Seq.collect (fun b -> seq { int b / 16; int b % 16 } )
    |> Seq.cast<int<Byte>>
    |> Seq.fold (fun (l, c, (n3: int<Byte> option), (n5: int<Byte> Set)) b ->
      if b <> l then
        (b, 1, n3, n5)
      else
        match c+1 with
        | 3  -> (b, c+1, Some (defaultArg n3 b), n5)
        | 5  -> (b, c+1, n3, n5.Add(b))
        | _  -> (b, c+1, n3, n5)) (-1<Byte>, 0, None, Set.empty)
  let n3s' =
    match n3 with
    | None    -> cache.n3s
    | Some n3 -> cache.n3s |> addSingle x n3
  let n5s' =
    match n5 |> Set.toList with
    | []  -> cache.n5s
    | n5s -> cache.n5s |> addMany x n5s
  grow fHash max { cache with n3s = n3s'; n5s = n5s'; ubound = x }

let rec calc (fHash: HashFunc) cache x = seq {
  let cache' = cache |> grow fHash (x + 1000<Index>)
  match cache'.n3s |> Seq.tryFind (fun k -> k.Value.Contains(x)) with
  | None   -> ()
  | Some r ->
    match cache'.n5s.TryFind(r.Key) with
    | None     -> ()
    | Some n5s ->
      let matches =
        n5s
        |> Seq.filter ((<) x)
        |> Seq.filter ((>=) (x+1000<Index>))
      match matches |> Seq.isEmpty with
      | false -> yield x
      | true  -> ()
  yield! calc fHash cache' (x + 1<Index>)
}

#time
let answer =
  calc (singleMd5 "ahsbgdzn") (Cache.create) 0<Index>
  |> Seq.take 64
  |> Seq.last
#time

let byteMap = dict [
  for b in {0..255} do
    let s = Convert.ToString(b, 16).PadLeft(2, '0')
    ( byte b, enco.GetBytes(s) )
  ]

let convertBytes (bytes: byte[]) =
  bytes |> Array.collect (fun b -> byteMap.Item(b))

let stretch (salt: string) (x: int<Index>) =
  let rec loop n (b: byte[]) =
    if n = 0 then b else
    loop (n-1) (b |> convertBytes |> md5.ComputeHash)
  loop 2016 (singleMd5 salt x)


#time
let answer' =
  calc (stretch "ahsbgdzn") (Cache.create) 0<Index>
  |> Seq.take 64
  |> Seq.last
#time
//Real: 00:02:26.347, CPU: 00:02:26.078, GC gen0: 22286, gen1: 16, gen2: 2
//val answer' : int<Index> = 22696

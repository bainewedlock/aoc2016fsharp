open System.Security.Cryptography
open System.Text

let salt = "ahsbgdzn"
let enco = Encoding.ASCII
let md5 = MD5.Create()

[<Measure>] type Index
[<Measure>] type Byte

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

let rec grow max cache =
  match cache.ubound + 1<Index> with
  | x when x > max -> cache
  | x ->
  let (_,_,n3,n5) =
    sprintf "%s%d" salt x
    |> enco.GetBytes
    |> md5.ComputeHash
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
  grow max { cache with n3s = n3s'; n5s = n5s'; ubound = x }

let rec calc cache x = seq {
  let cache' = cache |> grow (x + 1000<Index>)
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
  yield! calc cache' (x + 1<Index>)
}

let answer =
  calc (Cache.create) 0<Index>
  |> Seq.take 64
  |> Seq.last

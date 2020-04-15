open System
open FSharpPlus

// ---- ---- ZDD Model ---- ----
[<Struct>]
type Node<'T, 'E> =
  | Node of elm:'T * cfg:'E
  | Leaf of bool
[<Struct>]
type Path<'T, 'E> =
  { head : Node<'T, 'E>
    tag : bool
    tail : Node<'T, 'E> }

type ZddModel<'T, 'E when 'T : comparison
                      and 'E : comparison> =
  { elements : 'T list
    lengthElements : int
    memoDP : Set<Node<'T, 'E>> list
    pathes : Path<'T, 'E> list
    childCfg : ZddModel<'T, 'E> -> Node<'T, 'E> -> bool -> Node<'T, 'E> }

module ZddModel =
  let init elms rootCfg childCfg =
    let n = List.length elms
    let memo = List.append [Set.add (rootCfg elms) Set.empty] <| List.replicate (n-1) Set.empty
    { elements = elms
      lengthElements = n
      memoDP = memo
      pathes = []
      childCfg = childCfg}

  let constract model =
    let rec go i model =
      if model.lengthElements > i then
        let setCurrent = List.item i model.memoDP
        let searchSet s model =
          match s with
          | Leaf _ -> model
          | s ->
            let searchBranch x model =
              match model.childCfg model s x with
              | Leaf(a) -> 
                let pathes = {head=s; tag=x; tail=Leaf a}::model.pathes
                {model with pathes = pathes}
              | Node(e, cfg) ->
                let setNext = model.memoDP |> List.item (i+1) |> Set.add (Node (e,cfg))
                let memoDP = model.memoDP |> mapi (fun a x -> if a = i+1 then setNext else x)
                let pathes = {head=s; tag=x; tail=Node(e,cfg)}::model.pathes
                {model with memoDP = memoDP; pathes = pathes}

            model |> searchBranch false |> searchBranch true

        let model = setCurrent |> Set.toList |> fold (fun a b -> a |> searchSet b) model
        go (i+1) model
      else
        model

    go 0 model


// ---- ---- Knapsack problem ---- ----
[<Struct>]
type Element =
  { weight : int
    value : int }

[<Struct>]
type Cfg =
  { height : int
    weightSum : int
    count : int}

let search model =
  let pathes = model.pathes
  let f b e = function
    | {head=Node(e2, cfg); tag=b2} when e=Node(e2,cfg) && b=b2 -> true
    | _ -> false

  let rec go w ws =
    match w with
    | Leaf false -> Int32.MinValue, ws
    | Leaf true -> 0, ws
    | Node(elm, _) ->
      let falseBranch = pathes |> List.find (f false w) |> (fun x -> x.tail)
      let trueBranch = pathes |> List.find (f true w) |> (fun x -> x.tail)
      let (a,wsa),(b,wsb) = go falseBranch ws, go trueBranch ws
      if a > b+elm.value
      then a, wsa
      else b+elm.value, elm::wsb

  let e0 = List.head model.elements
  let init = pathes
             |> List.find (function | {head=Node(e2, _)} when e0=e2 -> true | _ -> false)
             |> (fun x -> x.head)
  go init []

// ---- ---- ---- ---- ---- ----


[<EntryPoint>]
let main argv =
  let elements =
    [ {value=3; weight=4}
      {value=4; weight=4}
      {value=5; weight=5} ]
  let limitWeight = 8
  let limitCount = 2

  let rootCfg elms =
    Node (List.head elms, {height=0;weightSum=0;count=0})
 
  let childCfg model s x =
    match s,x with
    | Node(e, cfg), true when cfg.height+1 = model.lengthElements ->
      if limitWeight >= cfg.weightSum + e.weight
      then Leaf true
      else Leaf false

    | Node(_, cfg), false when cfg.height+1 = model.lengthElements ->
      if limitWeight >= cfg.weightSum
      then Leaf true
      else Leaf false

    | Node(e, cfg), true ->
      let a = List.item (cfg.height+1) model.elements
      if  limitWeight < cfg.weightSum + e.weight
      then Leaf false
      else
        let cfg = { cfg with
                        height=cfg.height+1
                        weightSum=cfg.weightSum + e.weight }
        Node(a, cfg)
          
    | Node(_, cfg), false ->
      let a = List.item (cfg.height+1) model.elements
      let cfg = { cfg with
                      height=cfg.height+1
                      weightSum=cfg.weightSum }
      Node(a, cfg)

    | _, _ -> s


  let zdd =
    ZddModel.init elements rootCfg childCfg
    |> ZddModel.constract

  printfn "limitWeight: %d" limitWeight
  //printfn "limitCount: %d" limitCount
  printfn "elements:"
  List.iteri (fun i a -> printfn "w%d: weight=%d, value=%d" i a.weight a.value) zdd.elements
  //printfn "memoDP:"
  //List.iteri (fun i a -> printfn "N%d = %A" i a) zdd.memoDP
  printfn "pathes:"
  List.iteri (fun i {head=h; tag=b; tail=t} ->
                let f x =
                  match x with
                  | Leaf false -> "[0]" | Leaf true -> "[1]"
                  | Node(e,cfg) ->
                    "w"
                    + string cfg.height
                    //+ string (List.findIndex (fun x -> e=x) zdd.elements)
                    + "(" + string cfg.weightSum + ")"
                let hi, ti = f h, f t
                let arrow = if b then "---t--->" else "---f--->"
                printfn "%s" ((string i)+": "+hi+arrow+ti))
             <| List.rev zdd.pathes
  //let a,ws = search zdd
  //printfn "max value: %d." a
  //printfn "and path: %A" ws

  0 // return an integer exit code

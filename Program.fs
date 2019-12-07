// Learn more about F# at http://fsharp.org

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

type ZddModel<'T, 'E when 'T : comparison and 'E : comparison> =
  { elements : 'T list
    lengthElements : int
    memoDP : Set<Node<'T, 'E>> list
    pathes : (Node<'T, 'E> * bool * Node<'T, 'E>) list
    childCfg : ZddModel<'T, 'E> -> Node<'T, 'E> -> bool -> Node<'T, 'E> }

module ZddModel =
  let init elms rootCfg childCfg =
    let n = List.length elms
    let memo0 = Set.add (rootCfg elms) Set.empty
    let memoOther = List.replicate (n-1) Set.empty
    { elements = elms
      lengthElements = n
      memoDP = memo0::memoOther
      pathes = []
      childCfg = childCfg}

  let constract model =
    let inline g f a b = a |> f b
    let rec f model i =
      if model.lengthElements > i then
        let setCurrent = List.item i model.memoDP
        let searchSet s model =
          match s with
          | Leaf _ ->
            model
          | s ->
            let searchBranch x model =
              match model.childCfg model s x with
              | Leaf(a) -> 
                let pathes = (s, x, Leaf a)::model.pathes
                {model with pathes = pathes}
              | Node(e, cfg) ->
                let setNext = List.item (i+1) model.memoDP
                let setNext = Set.add (Node (e,cfg)) setNext
                let memoDP = mapi (fun a x -> if a = i+1 then setNext else x) model.memoDP
                let pathes = (s, x, Node(e,cfg))::model.pathes
                {model with memoDP = memoDP; pathes = pathes}

            model |> searchBranch false |> searchBranch true

        let model = fold (g searchSet) model <| Set.toList setCurrent
        f model (i+1)
      else
        model

    f model 0


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
  let f b e =
    function
    | (Node(e2, cfg), b2, _) when e=Node(e2,cfg) && b=b2 -> true
    | _ -> false

  let rec iter w ws =
    match w with
    | Leaf false -> Int32.MinValue, ws
    | Leaf true -> 0, ws
    | Node(elm, _) ->
      let falseB = (fun (_,_,tl) -> tl) <| List.find (f false w) pathes
      let trueB = (fun (_,_,tl) -> tl) <| List.find (f true w) pathes
      let (a,wsa),(b,wsb) = iter falseB ws, iter trueB ws
      if a > b+elm.value
      then a, wsa
      else b+elm.value, elm::wsb

  let e0 = List.head model.elements
  let init = (fun (h,_,_) -> h) <| List.find (function
                                             | (Node(e2,_),_,_) when e0=e2 -> true
                                             | _ -> false)
                                           pathes
  iter init []

// ---- ---- ---- ---- ---- ----


[<EntryPoint>]
let main argv =
  let elements =
    [ {value=981421680; weight=325}
      {value=515936168; weight=845}
      {value=17309336; weight=371}
      {value=788067075; weight=112}
      {value=104855562; weight=96}
      {value=494541604; weight=960}
      {value=32007355; weight=161}
      {value=772339969; weight=581}
      {value=55112800; weight=248}
      {value=98577050; weight=22}]
  let limitWeight = 2921
  let limitCount = 2

  let rootCfg elms =
    Node (List.head elms, {height=0;weightSum=0;count=0})
 
  let childCfg model s x =
    let belowLimitWeight model s x =
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

    let eqLimitCount model s x =
      match s,x with
      | Node(_,cfg), true when cfg.height+1 = model.lengthElements ->
        if limitCount = cfg.count+1
        then Leaf true
        else Leaf false
      | Node(_,cfg), false when cfg.height+1 = model.lengthElements ->
        if limitCount = cfg.count
        then Leaf true
        else Leaf false
      | Node(_,cfg), true ->
        let a = List.item (cfg.height+1) model.elements
        let cfg = { cfg with
                      height=cfg.height
                      count=cfg.count+1 }
        Node(a, cfg)
      | Node(_,cfg), false ->
        let a = List.item (cfg.height+1) model.elements
        let cfg = { cfg with
                      height=cfg.height
                      count=cfg.count}
        Node(a, cfg)
      | _, _ -> s

    //match belowLimitWeight model s x, eqLimitCount model s x with
    //| Leaf b1, Leaf b2 -> Leaf (b1 && b2)
    //| Node(e,cfg1), Node(_,cfg2) ->
    //  let cfg = { cfg1 with count=cfg2.count }
    //  Node(e, cfg)
    //| Node(e, cfg), _ | _, Node(e,cfg) ->
    //  Node(e, cfg)

    belowLimitWeight model s x


  let zdd =
    ZddModel.init elements rootCfg childCfg
    |> ZddModel.constract

  printfn "limitWeight: %d" limitWeight
  printfn "limitCount: %d" limitCount
  printfn "elements:"
  List.iteri (fun i a -> printfn "w%d: weight=%d, value=%d" i a.weight a.value) zdd.elements
  //printfn "memoDP:"
  //List.iteri (fun i a -> printfn "N%d = %A" i a) zdd.memoDP
  //printfn "pathes:"
  //List.iteri (fun i (h,b,t) ->
  //              let f x =
  //                match x with
  //                | Leaf false -> "[0]" | Leaf true -> "[1]"
  //                | Node(e,cfg) ->
  //                  "w"
  //                  + string cfg.height
  //                  //+ string (List.findIndex (fun x -> e=x) zdd.elements)
  //                  + "(" + string cfg.weightSum + ")"
  //              let hi, ti = f h, f t
  //              let arrow = if b then "---t--->" else "---f--->"
  //             printfn "%s" ((string i)+": "+hi+arrow+ti))
  //           <| List.rev zdd.pathes
  let a,ws = search zdd
  printfn "max value: %d." a
  printfn "and path: %A" ws

  0 // return an integer exit code

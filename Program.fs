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
    weightSum : int }

// ---- ---- ---- ---- ---- ----

[<EntryPoint>]
let main argv =
  let elements = [ {weight=4;value=10}; {weight=4;value=15}; {weight=5;value=20} ]
  let limit = 8

  let rootCfg elms =
    Node (List.head elms, {height=0;weightSum=0})

  let childCfg model s x =
    match s,x with
    | Node(e, cfg), true when cfg.height+1 = model.lengthElements ->
      if cfg.weightSum + e.weight > limit
      then Leaf false else Leaf true
    | Node(_, cfg), false when cfg.height+1 = model.lengthElements ->
      if cfg.weightSum > limit
      then Leaf false else Leaf true
    | Node(e, cfg), true ->
      let a = List.item (cfg.height+1) model.elements
      if cfg.weightSum + e.weight > limit
      then Leaf false
      else
        let cfg = {height=cfg.height+1; weightSum=cfg.weightSum + e.weight}
        Node(a, cfg)
    | Node(_, cfg), false ->
      let a = List.item (cfg.height+1) model.elements
      let cfg = {height=cfg.height+1; weightSum=cfg.weightSum}
      Node(a, cfg)
    | _, _ -> s

  let zdd =
    ZddModel.init elements rootCfg childCfg
    |> ZddModel.constract

  printfn "limit: %d" limit
  printfn "elements:"
  List.iteri (fun i a -> printfn "w%d: weight=%d, value=%d" i a.weight a.value) zdd.elements
  printfn "memoDP:"
  List.iteri (fun i a -> printfn "N%d = %A" i a) zdd.memoDP
  printfn "pathes:"
  List.iteri (fun i (h,b,t) ->
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

  0 // return an integer exit code

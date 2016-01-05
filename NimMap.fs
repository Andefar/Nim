module NimMap

open System
open System.Drawing
open System.Windows.Forms

type NimMap = N of Map<string,int>

let make n                    = N(Map.ofList (List.init 3 (fun i -> (string (i+1),n))))
let update h removed (N heap) = N(Map.add h removed (Map.filter (fun k v -> k<>h) heap))
let find h (N heap)           = Map.find h heap
let getM (N heap)             = Map.fold (fun state _ value -> state ^^^ value) 0 heap
let findKey m (N heap)        = Map.findKey (fun key value -> value ^^^ m < value) heap
let findMax (N heap)          = List.maxBy snd (Map.toList heap)
let getMap (N heap)           = heap
let win (N heap)              = Map.forall (fun _ value -> value = 0) heap


let calcOptimal map =
    let m = getM map
    if(m <> 0) then
        let ak_xorb_m_key =  findKey m map
        let ak_xorb_m = find ak_xorb_m_key map
        let sub = ak_xorb_m ^^^ m
        let diff = ak_xorb_m - sub
        (ak_xorb_m_key,sub)
     else
        let chosenHeap = findMax map
        let sub = (snd chosenHeap) - 1
        ((fst chosenHeap),sub)
﻿#load "Gui.fs"

(*
Arthurs: Andreas & Silas
Januarkursus - 2015
*)

// Prelude
open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 

// An asynchronous event queue kindly provided by Don Syme 
type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() = 
        match queue.Count, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)



// Automaton, Controller
type heap = int * char
type Message = 
    | Input of heap | Clear | Cancel | IllegalInput| Legal


let input = 0;;
let heap1 = (10,'1');;
let heap2 = (10,'2');;
let heap3 = (10,'3');;

let heaps = [heap1;heap2;heap3]
let eventQ = AsyncEventQueue()

let rec initGame() =
    async{printf("Init")
          List.iter (fun (i,n) -> Gui.updateHeap i n) heaps
          
          return! ready()}

and ready() = 
    async {printf("Ready")
           Gui.disable [Gui.newGame]

           let! input = eventQ.Receive()
           match input with
           | Input (i,h)    -> return! checkInput i h 
           | Clear          -> return! ready()
           | _              -> failwith("Ready: You fool")}

and checkInput i h = 
    async {printf("checkInput") 
           Gui.updateHeap i h
           
           // TODO: Check input
           return! checkStatus()}

and checkStatus() = 
    async {printf("checkStatus")
           return! ready()
//           if (List.forall (fun x -> x=0) [n1;n2;n3]) then finished()
//           else computer()
           }

//and finished() = 
//    async {initGame()
//           return! ready()}

//and computer() = 
//    async {Gui.UpdateHeap n1 heap1
//           Gui.UpdateHeap n2 heap2
//           Gui.UpdateHeap n3 heap3
//           
//           return! ready()}

let addListeners = 
    Gui.heap1.Click.Add (fun _ -> eventQ.Post (Input (int(Gui.input.Text),'1')))
    Gui.heap2.Click.Add (fun _ -> eventQ.Post (Input (int(Gui.input.Text),'2')))
    Gui.heap3.Click.Add (fun _ -> eventQ.Post (Input (int(Gui.input.Text),'3')))

let startGame =
    Gui.initGui
    addListeners
    Gui.window.Show()
    Async.StartImmediate (initGame())

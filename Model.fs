module Model

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
type Message = 
    | Input of int | Clear | Cancel | IllegalInput| Legal

let input = 0;;
let heap1 = '1';;
let heap2 = '2';;
let heap3 = '3';;

let eventQ = AsyncEventQueue()



let rec initGame() =
    async{Gui.updateHeap 10 heap1
          Gui.updateHeap 10 heap2
          Gui.updateHeap 10 heap3
          
          return! ready()}

and ready() = 
    async {Gui.updateHeap n1 heap1
           Gui.updateHeap n2 heap2
           Gui.updateHeap n3 heap3

           Gui.disable [Gui.newGame]

           let! input = eventQ.Receive()
           match input with
           | Input i    -> return! checkInput(i)
           | Clear      -> return! ready()
           | _          -> failwith("Ready: You fool")}

and checkInput(i,h) = 
    async {
//           Gui.UpdateInputBox "Checking input"
           
           // TODO: Check input
           return! ready()}

and checkStatus() = 
    async {if (List.forall (fun x -> x=0) [heap1;heap2;heap3]) then finished()
           else computer()
           }

and finished() = 
    async {Gui.UpdateHeap n1 heap1
           Gui.UpdateHeap n2 heap2
           Gui.UpdateHeap n3 heap3
           
           let! msg = eventQ.Receive()
           match msg with
           | NewGame -> return! initGame()
           | _       -> failwith "finished: You fool!"
    }
and computer() = 
    async {Gui.UpdateHeap n1 heap1
           Gui.UpdateHeap n2 heap2
           Gui.UpdateHeap n3 heap3
           
           return! ready()}

let postQ heap input = eventQ.Post (checkInput heap input)
 

#load "Gui.fs"

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

let printCollection msg coll =
        printfn "%s:" msg
        Seq.iteri (fun index item -> printfn "  %i: %O" index item) coll




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
type heap = string * int
type Message = 
    | Input of heap | Clear | Cancel | IllegalInput | Legal | Restart

let restartMap = 
    let heap1 = ("1",10)
    let heap2 = ("2",10)
    let heap3 = ("3",10)
    Map.ofList [heap1;heap2;heap3]

let mutable heaps = restartMap


let updateHeapsMap h removed =
    Map.add h removed (Map.filter (fun k v -> k<>h) heaps)

let checkValidInputAndMove h i map  = 
    let curMatches = (Map.find h heaps) - i

    if (curMatches >= 0 && i >= 0) then 
         Gui.updateHeap h curMatches
         heaps <- updateHeapsMap h curMatches
         Legal
     else
        Gui.printMessage "Input is invalid :P go again.."
        IllegalInput


let moveComp heapsmap = 
    Gui.printMessage "Now the computer will make its move! xD"
    let m = Map.fold (fun state _ value -> state ^^^ value) 0 heapsmap
    if(m <> 0) then
        let ak_xorb_m_key =  (Map.findKey (fun key value -> value ^^^ m < value) heapsmap)
        let ak_xorb_m = Map.find ak_xorb_m_key heapsmap
        let sub = ak_xorb_m ^^^ m
        let diff = ak_xorb_m - sub
        Gui.showInput diff.ToString
        Thread.Sleep(2000)
        Gui.updateHeap h curMatches
        heaps <- updateHeapsMap h curMatches

    else



let mutable first = true
let mutable playersTurn = true

let eventQ = AsyncEventQueue()
let rec initGame() =
    async{//printf("Init\n")
          Gui.printMessage "Welcome! Enter number of matches you want to remove and select a heap"
          Gui.enable [Gui.heap1;Gui.heap2;Gui.heap3];
          heaps <- restartMap
          first <- true
          Map.iter (fun key value -> Gui.updateHeap key value) heaps
          Gui.disable [Gui.newGame]
          return! ready()}

and ready() = 
    async {//printf("Ready\n")
           if not first then Gui.printMessage "Great move! ;)"
           let! input = eventQ.Receive()
           match input with
           | Input (h,i)    -> return! checkInput h i 
           | Clear          -> return! ready()
           | _              -> failwith("Ready: You fool")}

and checkInput h i = 
    async {//printf("checkInput\n")
           first <- false
           playersTurn <- false
           let input = checkValidInputAndMove h i heaps
           match input with
           | Legal          -> return! checkStatus()
           | IllegalInput   -> return! ready()
           | _              -> failwith "checkinput failed"}



and checkStatus() = 
    async {//printf("checkStatus\n")
           printCollection "heaps" heaps
           if (Map.forall (fun _ value -> value = 0) heaps) then return! finished()
           elif playersTurn then return! ready()
           else return! computer()
           }

and finished() = 
    async {//printf("finished\n")
           Gui.printMessage "xxxxxx WON! :D wanna try again? hit the \"Start New Game\""
           Gui.enable [Gui.newGame]
           Gui.disable [Gui.heap1;Gui.heap2;Gui.heap3];
           let! input = eventQ.Receive()
           match input with
           | Restart    -> return! initGame()
           | _          -> failwith("finished: You fool")}
          
and computer() = 
    async {//TODO: implement computers algorithm in movecomp function (see above)
           playersTurn <- true
           let input = moveComp heaps
           match input with
           | Legal    -> return! checkStatus()
           | _        -> failwith("computers move fail: You fool")}

let addListeners = 
    Gui.heap1.Click.Add (fun _ -> eventQ.Post (Input ("1",int(Gui.input.Text))))
    Gui.heap2.Click.Add (fun _ -> eventQ.Post (Input ("2",int(Gui.input.Text))))
    Gui.heap3.Click.Add (fun _ -> eventQ.Post (Input ("3",int(Gui.input.Text))))
    Gui.newGame.Click.Add (fun _ -> eventQ.Post Restart)

let startGame =
    Gui.initGui
    addListeners
    //Gui.window.Show()
    Async.StartImmediate (initGame())
    Application.Run(Gui.window)

 8 ^^^ 4 ^^^ 3

8 ^^^ 15
(* M != 0 
1. m = 15
2. find ak_xorb_m ---> 8
3. find ak_xorb_m_key ---> 1
4. let sub = ak_xorb_m ^^^ m = 7
5. let diff = ak_xorb_m - sub = 1

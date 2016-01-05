#load "Gui.fs"

(*
Authors: Andreas & Silas
Januarkursus - 2015
*)

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



// Automaton
type heap = string * int
type Message = 
    | Input of heap | Clear | Cancel | IllegalInput | Legal | Restart | Hint

let restartMap = 
    let heap1 = ("1",12)
    let heap2 = ("2",12)
    let heap3 = ("3",12)
    Map.ofList [heap1;heap2;heap3]

let mutable heaps = restartMap
let mutable first = true
let mutable playersTurn = true
let mutable lastComHeap = ""
let mutable lastComValue = ""
let mutable hintsLeft = 3


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


let calcOptimal heapsmap =
    let m = Map.fold (fun state _ value -> state ^^^ value) 0 heapsmap
    if(m <> 0) then
        let ak_xorb_m_key =  (Map.findKey (fun key value -> value ^^^ m < value) heapsmap)
        let ak_xorb_m = Map.find ak_xorb_m_key heapsmap
        let sub = ak_xorb_m ^^^ m
        let diff = ak_xorb_m - sub
        (ak_xorb_m_key,diff)
     else
        let chosenHeap = (List.head (Map.toList (Map.filter (fun key value -> value <> 0) heapsmap)))
        let rnd = System.Random()
        let sub = rnd.Next(snd chosenHeap)
        let diff = (snd chosenHeap) - sub
        ((fst chosenHeap),diff)


let moveComp heapsmap = 

    let m = Map.fold (fun state _ value -> state ^^^ value) 0 heapsmap
    if(m <> 0) then
        let ak_xorb_m_key =  (Map.findKey (fun key value -> value ^^^ m < value) heapsmap)
        let ak_xorb_m = Map.find ak_xorb_m_key heapsmap
        let sub = ak_xorb_m ^^^ m
        let diff = ak_xorb_m - sub
        Gui.updateHeap ak_xorb_m_key sub
        heaps <- updateHeapsMap ak_xorb_m_key sub
        lastComValue <- string diff
        lastComHeap <- ak_xorb_m_key
        Legal
    else
        let chosenHeap = (List.head (Map.toList (Map.filter (fun key value -> value <> 0) heapsmap)))
        let rnd = System.Random()
        let sub = rnd.Next(snd chosenHeap)
        let diff = (snd chosenHeap) - sub
        Gui.updateHeap (fst chosenHeap) sub
        heaps <- updateHeapsMap (fst chosenHeap) sub
        lastComValue <- string diff
        lastComHeap <- (fst chosenHeap)
        Legal


let eventQ = AsyncEventQueue()
let rec initGame() =
    async{//printf("Init\n")
          Gui.printMessage "Welcome! Enter number of matches you want to remove and select a heap"
          Gui.enable [Gui.heap1;Gui.heap2;Gui.heap3];
          heaps <- restartMap
          first <- true
          hintsLeft <- 3
          Gui.showHintsLeft ("hint("+(string hintsLeft)+")")
          Map.iter (fun key value -> Gui.updateHeap key value) heaps
          Gui.disable [Gui.newGame]
          return! ready()}

and ready() = 
    async {//printf("Ready\n")
           if not first && playersTurn then Gui.printMessage ("The computer removed "+lastComValue+" matches from heap "+lastComHeap)
           let! input = eventQ.Receive()
           match input with
           | Input (h,i)    -> return! checkInput h i 
           | Clear          -> return! ready()
           | Hint           ->  let opt = calcOptimal heaps

                                if hintsLeft > 0 then Gui.showHint ("h"+(string (fst opt)+" m"+(string (snd opt))))
                                else Gui.showHint ""
                                if hintsLeft>0 then hintsLeft <- hintsLeft-1
                                Gui.showHintsLeft ("hint("+(string hintsLeft)+")")
                                return! ready()
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
           //printCollection "heaps" heaps
           Gui.showInput ""
           Gui.showHint ""
           if (Map.forall (fun _ value -> value = 0) heaps) then return! finished()
           elif playersTurn then
                Gui.enable [Gui.heap1;Gui.heap2;Gui.heap3]
                return! ready()
           else return! computer()
           }

and finished() = 
    async {//printf("finished\n")
           let mutable winner = "You"
           if playersTurn then winner <- "The computer"
           Gui.printMessage (winner+" WON! :D wanna try again? hit the \"Start New Game\"")
           Gui.enable [Gui.newGame]
           Gui.disable [Gui.heap1;Gui.heap2;Gui.heap3];
           let! input = eventQ.Receive()
           match input with
           | Restart    -> return! initGame()
           | _          -> failwith("finished: You fool")}
          
and computer() = 
    async {//printf("computer\n")
           Gui.disable [Gui.heap1;Gui.heap2;Gui.heap3]
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
    Gui.hintBut.Click.Add (fun _ -> eventQ.Post Hint)

let startGame =
    Gui.initGui
    addListeners
    //Gui.window.Show()
    Async.StartImmediate (initGame())
    Application.Run(Gui.window)
 
(*
TODO list: 
    - fix Gui.disable så man ikke kan trykke på knapper selvom man har disabled dem
    - samle redundant kode i metode (calcOptimal and moveComp)
    - tjek om computer strategi er korrekt mht. om random valgte træk
    - tjek om kan man vælge 0?
    - GUI: samle show metoderne til én metode
    - Lave andre extension
    - fix regel 2 i computerens strategi
    - lav computer ryk til async (push/receive) (l. 187)
*)
